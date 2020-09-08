/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997-2018  The R Core Team
 *  Copyright (C) 2003-2018  The R Foundation
 *  Copyright (C) 1995,1996  Robert Gentleman, Ross Ihaka
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

/** @file coerce.cpp
 *
 * Coercions from one R type to another, including various 'is' and
 * 'as' functions.  Also 'quote'.
 */

#define R_NO_REMAP

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif


#include <Defn.h> /*-- Maybe modularize into own Coerce.h ..*/
#include <Localization.h>
#include <Internal.h>
#include <float.h> /* for DBL_DIG */
#define R_MSG_mode	_("invalid 'mode' argument")
#define R_MSG_list_vec	_("applies only to lists and vectors")
#include <Rmath.h>
#include <Print.h>
#include "rho/ArgMatcher.hpp"
#include "rho/Expression.hpp"
#include "rho/ExpressionVector.hpp"
#include "rho/GCStackRoot.hpp"
#include "rho/Promise.hpp"
#include "rho/BuiltInFunction.hpp"
#include "rho/unrho.hpp"

using namespace std;
using namespace rho;

/* interval at which to check interrupts */
constexpr R_xlen_t NINTERRUPT = 10000000;

/* This section of code handles type conversion for elements */
/* of data vectors.  Type coercion throughout R should use these */
/* routines to ensure consistency. */

/* Coercion warnings will be OR'ed : */
#define WARN_NA	   1
#define WARN_INT_NA 2
#define WARN_IMAG  4
#define WARN_RAW  8

/* The following two functions copy or clear the attributes.  They
   avoid calling the assignment functions when possible, since the
   write barrier (and possibly cache behavior on some architectures)
   makes assigning more costly than dereferencing. */
namespace {
    inline void cDUPLICATE_ATTRIB(SEXP to, SEXP from)
    {
	if (ATTRIB(from)) SHALLOW_DUPLICATE_ATTRIB(to, from);
    }

    inline void CLEAR_ATTRIB(SEXP x)
    {
	if (ATTRIB(x)) {
	    x->clearAttributes();
	    if (IS_S4_OBJECT(x)) UNSET_S4_OBJECT(x);
	}
    }
}

HIDDEN void Rf_CoercionWarning(int warn)
{
/* FIXME: Use
   =====
   WarningMessage(nullptr, WARNING_....);
*/
    if (warn & WARN_NA)
	Rf_warning(_("NAs introduced by coercion"));
    if (warn & WARN_INT_NA)
	Rf_warning(_("NAs introduced by coercion to integer range"));
    if (warn & WARN_IMAG)
	Rf_warning(_("imaginary parts discarded in coercion"));
    if (warn & WARN_RAW)
	Rf_warning(_("out-of-range values treated as 0 in coercion to raw"));
}

HIDDEN int Rf_LogicalFromInteger(int x, int *warn)
{
    return (x == R_NaInt) ?
	R_NaLog : (x != 0);
}

HIDDEN int Rf_LogicalFromReal(double x, int* warn)
{
    return std::isnan(x) ? R_NaLog : (x != 0);
}

HIDDEN int Rf_LogicalFromComplex(Rcomplex x, int* warn)
{
    return (std::isnan(x.r) || std::isnan(x.i)) ? R_NaLog : (x.r != 0 || x.i != 0);
}

HIDDEN int Rf_LogicalFromString(SEXP x, int* warn)
{
    if (x != R_NaString) {
	if (Rf_StringTrue(R_CHAR(x)))
	    return 1;
	if (Rf_StringFalse(R_CHAR(x)))
	    return 0;
    }
    return R_NaLog;
}

HIDDEN int Rf_IntegerFromLogical(int x, int* warn)
{
    return (x == R_NaLog) ? R_NaInt : x;
}

HIDDEN int Rf_IntegerFromReal(double x, int* warn)
{
    if (std::isnan(x))
	return R_NaInt;
    else if (x >= INT_MAX + 1. || x <= INT_MIN) {
	*warn |= WARN_INT_NA;
	return R_NaInt;
    }
    return int(x);
}

HIDDEN int Rf_IntegerFromComplex(Rcomplex x, int* warn)
{
    if (std::isnan(x.r) || std::isnan(x.i))
	return R_NaInt;
    else if (x.r > INT_MAX + 1. || x.r <= INT_MIN) {
	*warn |= WARN_INT_NA;
	return R_NaInt;
    }
    if (x.i != 0)
	*warn |= WARN_IMAG;
    return int(x.r);
}

HIDDEN int Rf_IntegerFromString(SEXP x, int *warn)
{
    double xdouble;
    char *endp;
    if (x != R_NaString && !Rf_isBlankString(R_CHAR(x))) { /* ASCII */
	xdouble = R_strtod(R_CHAR(x), &endp); /* ASCII */
	if (Rf_isBlankString(endp)) {
#ifdef _R_pre_Version_3_3_0
	    if (xdouble > INT_MAX) {
		*warn |= WARN_INT_NA;
		return INT_MAX;
	    }
	    else if(xdouble < INT_MIN+1) {
		*warn |= WARN_INT_NA;
		return INT_MIN;// <- "wrong" as INT_MIN == R_NaInt currently; should have used INT_MIN+1
	    }
#else
	    // behave the same as IntegerFromReal() etc:
	    if (xdouble >= INT_MAX+1. || xdouble <= INT_MIN ) {
		*warn |= WARN_INT_NA;
		return R_NaInt;
	    }
#endif
	    else
		return int(xdouble);
	}
	else *warn |= WARN_NA;
    }
    return R_NaInt;
}

HIDDEN double Rf_RealFromLogical(int x, int* warn)
{
    return (x == R_NaLog) ? R_NaReal : x;
}

HIDDEN double Rf_RealFromInteger(int x, int* warn)
{
    if (x == R_NaInt)
	return R_NaReal;
    else
	return x;
}

HIDDEN double Rf_RealFromComplex(Rcomplex x, int* warn)
{
    if (std::isnan(x.r) || std::isnan(x.i))
	return R_NaReal;
    if (std::isnan(x.r))
	return x.r;
    if (std::isnan(x.i))
	return R_NaReal;
    if (x.i != 0)
	*warn |= WARN_IMAG;
    return x.r;
}

HIDDEN double Rf_RealFromString(SEXP x, int* warn)
{
    double xdouble;
    char* endp;
    if (x != R_NaString && !Rf_isBlankString(R_CHAR(x))) { /* ASCII */
	xdouble = R_strtod(R_CHAR(x), &endp); /* ASCII */
	if (Rf_isBlankString(endp))
	    return xdouble;
	else
	    *warn |= WARN_NA;
    }
    return R_NaReal;
}

HIDDEN Rcomplex Rf_ComplexFromLogical(int x, int* warn)
{
    Rcomplex z;
    if (x == R_NaLog) {
	z.r = R_NaReal;
	z.i = R_NaReal;
    } else {
	z.r = x;
	z.i = 0;
    }
    return z;
}

HIDDEN Rcomplex Rf_ComplexFromInteger(int x, int* warn)
{
    Rcomplex z;
    if (x == R_NaInt) {
	z.r = R_NaReal;
	z.i = R_NaReal;
    } else {
	z.r = x;
	z.i = 0;
    }
    return z;
}

HIDDEN Rcomplex Rf_ComplexFromReal(double x, int* warn)
{
    Rcomplex z;
#ifdef PRE_R_3_3_0
    if (std::isnan(x)) {
	z.r = R_NaReal;
	z.i = R_NaReal;
    } else {
#endif
	z.r = x;
	z.i = 0;
#ifdef PRE_R_3_3_0
    }
#endif
    return z;
}

HIDDEN Rcomplex Rf_ComplexFromString(SEXP x, int *warn)
{
    double xr, xi;
    Rcomplex z;
    const char* xx = R_CHAR(x); /* ASCII */
    char* endp;

    z.r = z.i = R_NaReal;
    if (x != R_NaString && !Rf_isBlankString(xx)) {
	xr = R_strtod(xx, &endp);
	if (Rf_isBlankString(endp)) {
	    z.r = xr;
	    z.i = 0.0;
	}
	else if (*endp == '+' || *endp == '-') {
	    xi = R_strtod(endp, &endp);
	    if (*endp++ == 'i' && Rf_isBlankString(endp)) {
		z.r = xr;
		z.i = xi;
	    }
	    else *warn |= WARN_NA;
	}
	else *warn |= WARN_NA;
    }
    return z;
}

HIDDEN SEXP Rf_StringFromLogical(int x, int *warn)
{
    int w;
    formatLogical(&x, 1, &w);
    if (x == R_NaLog) return R_NaString;
    else return Rf_mkChar(EncodeLogical(x, w));
}

/* The conversions for small non-negative integers are saved in a chache. */
#define SFI_CACHE_SIZE 512
static SEXP sficache = nullptr;

HIDDEN SEXP Rf_StringFromInteger(int x, int *warn)
{
    if (x == R_NaInt) return R_NaString;
    else if (x >= 0 && x < SFI_CACHE_SIZE) {
	if (sficache == nullptr) {
	    sficache = Rf_allocVector(STRSXP, SFI_CACHE_SIZE);
	    R_PreserveObject(sficache);
	}
	SEXP cval = STRING_ELT(sficache, x);
	if (cval == R_BlankString) {
	    int w;
	    formatInteger(&x, 1, &w);
	    cval = Rf_mkChar(EncodeInteger(x, w));
	    SET_STRING_ELT(sficache, x, cval);
	}
	return cval;
    }
    else {
	int w;
	formatInteger(&x, 1, &w);
	return Rf_mkChar(EncodeInteger(x, w));
    }
}

// dropTrailing0 and StringFromReal moved to printutils.cpp

HIDDEN SEXP Rf_StringFromComplex(Rcomplex x, int *warn)
{
    int wr, dr, er, wi, di, ei;
    formatComplex(&x, 1, &wr, &dr, &er, &wi, &di, &ei, 0);
    if (ISNA(x.r) || ISNA(x.i)) // "NA" if Re or Im is (but not if they're just NaN)
	return R_NaString;
    else /* EncodeComplex has its own anti-trailing-0 care :*/
	return Rf_mkChar(EncodeComplex(x, wr, dr, er, wi, di, ei, OutDec));
}

static SEXP StringFromRaw(Rbyte x, int *warn)
{
    char buf[3];
    sprintf(buf, "%02x", x);
    return Rf_mkChar(buf);
}

/* Conversion between the two list types (LISTSXP and VECSXP). */

SEXP Rf_PairToVectorList(SEXP x)
{
    SEXP xptr, xnew, xnames;
    int i, len = 0, named = 0;
    for (xptr = x ; xptr != nullptr ; xptr = CDR(xptr)) {
	named = named | (TAG(xptr) != nullptr);
	len++;
    }
    PROTECT(x);
    PROTECT(xnew = Rf_allocVector(VECSXP, len));
    for (i = 0, xptr = x; i < len; i++, xptr = CDR(xptr)) {
	RAISE_NAMED(CAR(xptr), NAMED(x));
	SET_VECTOR_ELT(xnew, i, CAR(xptr));
    }
    if (named) {
	PROTECT(xnames = Rf_allocVector(STRSXP, len));
	xptr = x;
	for (i = 0, xptr = x; i < len; i++, xptr = CDR(xptr)) {
	    if(TAG(xptr) == nullptr)
		SET_STRING_ELT(xnames, i, R_BlankString);
	    else
		SET_STRING_ELT(xnames, i, PRINTNAME(TAG(xptr)));
	}
	Rf_setAttrib(xnew, Symbols::NamesSymbol, xnames);
	UNPROTECT(1);
    }
    Rf_copyMostAttrib(x, xnew);
    UNPROTECT(2);
    return xnew;
}

SEXP Rf_VectorToPairList(SEXP x)
{
    SEXP xptr, xnew, xnames;
    int i, len, named;

    len = Rf_length(x);
    PROTECT(x);
    PROTECT(xnew = Rf_allocList(len)); /* limited to int */
    PROTECT(xnames = Rf_getAttrib(x, Symbols::NamesSymbol));
    named = (xnames != nullptr);
    xptr = xnew;
    for (i = 0; i < len; i++) {
	RAISE_NAMED(VECTOR_ELT(x, i), NAMED(x));
	SETCAR(xptr, VECTOR_ELT(x, i));
	if (named && R_CHAR(STRING_ELT(xnames, i))[0] != '\0') /* ASCII */
	    SET_TAG(xptr, Rf_installTrChar(STRING_ELT(xnames, i)));
	xptr = CDR(xptr);
    }
    if (len > 0)       /* can't set attributes on NULL */
	Rf_copyMostAttrib(x, xnew);
    UNPROTECT(3);
    return xnew;
}

static SEXP coerceToSymbol(SEXP v)
{
    SEXP ans = nullptr;
    int warn = 0;
    if (Rf_length(v) <= 0)
	Rf_error(_("invalid data of mode '%s' (too short)"),
	      Rf_type2char(TYPEOF(v)));
    PROTECT(v);
    switch(TYPEOF(v)) {
    case LGLSXP:
	ans = Rf_StringFromLogical(LOGICAL_ELT(v, 0), &warn);
	break;
    case INTSXP:
	ans = Rf_StringFromInteger(INTEGER_ELT(v, 0), &warn);
	break;
    case REALSXP:
	ans = Rf_StringFromReal(REAL_ELT(v, 0), &warn);
	break;
    case CPLXSXP:
	ans = Rf_StringFromComplex(COMPLEX_ELT(v, 0), &warn);
	break;
    case STRSXP:
	ans = STRING_ELT(v, 0);
	break;
    case RAWSXP:
	ans = StringFromRaw(RAW_ELT(v, 0), &warn);
	break;
    default:
	UNIMPLEMENTED_TYPE("coerceToSymbol", v);
    }
    if (warn) Rf_CoercionWarning(warn);/*2000/10/23*/
    ans = Rf_installTrChar(ans);
    UNPROTECT(1);
    return ans;
}

static SEXP coerceToLogical(SEXP v)
{
    SEXP ans;
    int warn = 0;
    R_xlen_t i, n;
    PROTECT(ans = Rf_allocVector(LGLSXP, n = XLENGTH(v)));
    int *pa = LOGICAL(ans);
    ans->maybeTraceMemory(v);
    cDUPLICATE_ATTRIB(ans, v);
    switch (TYPEOF(v)) {
    case INTSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = Rf_LogicalFromInteger(INTEGER_ELT(v, i), &warn);
	}
	break;
    case REALSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = Rf_LogicalFromReal(REAL_ELT(v, i), &warn);
	}
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = Rf_LogicalFromComplex(COMPLEX_ELT(v, i), &warn);
	}
	break;
    case STRSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = Rf_LogicalFromString(STRING_ELT(v, i), &warn);
	}
	break;
    case RAWSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = Rf_LogicalFromInteger(int(RAW_ELT(v, i)), &warn);
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("coerceToLogical", v);
    }
    if (warn) Rf_CoercionWarning(warn);
    UNPROTECT(1);
    return ans;
}

static SEXP coerceToInteger(SEXP v)
{
    SEXP ans;
    int warn = 0;
    R_xlen_t i, n;
    PROTECT(ans = Rf_allocVector(INTSXP, n = XLENGTH(v)));
    int *pa = INTEGER(ans);
    ans->maybeTraceMemory(v);
    SHALLOW_DUPLICATE_ATTRIB(ans, v);
    switch (TYPEOF(v)) {
    case LGLSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = Rf_IntegerFromLogical(LOGICAL_ELT(v, i), &warn);
	}
	break;
    case REALSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = Rf_IntegerFromReal(REAL_ELT(v, i), &warn);
	}
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = Rf_IntegerFromComplex(COMPLEX_ELT(v, i), &warn);
	}
	break;
    case STRSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = Rf_IntegerFromString(STRING_ELT(v, i), &warn);
	}
	break;
    case RAWSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = int(RAW_ELT(v, i));
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("coerceToInteger", v);
    }
    if (warn) Rf_CoercionWarning(warn);
    UNPROTECT(1);
    return ans;
}

static SEXP coerceToReal(SEXP v)
{
    SEXP ans;
    int warn = 0;
    R_xlen_t i, n;
    PROTECT(ans = Rf_allocVector(REALSXP, n = XLENGTH(v)));
    double *pa = REAL(ans);
    ans->maybeTraceMemory(v);
    cDUPLICATE_ATTRIB(ans, v);
    switch (TYPEOF(v)) {
    case LGLSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = Rf_RealFromLogical(LOGICAL_ELT(v, i), &warn);
	}
	break;
    case INTSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = Rf_RealFromInteger(INTEGER_ELT(v, i), &warn);
	}
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = Rf_RealFromComplex(COMPLEX_ELT(v, i), &warn);
	}
	break;
    case STRSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = Rf_RealFromString(STRING_ELT(v, i), &warn);
	}
	break;
    case RAWSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = Rf_RealFromInteger(int(RAW_ELT(v, i)), &warn);
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("coerceToReal", v);
    }
    if (warn) Rf_CoercionWarning(warn);
    UNPROTECT(1);
    return ans;
}

static SEXP coerceToComplex(SEXP v)
{
    SEXP ans;
    int warn = 0;
    R_xlen_t i, n;
    PROTECT(ans = Rf_allocVector(CPLXSXP, n = XLENGTH(v)));
    Rcomplex *pa = COMPLEX(ans);
    ans->maybeTraceMemory(v);
    cDUPLICATE_ATTRIB(ans, v);
    switch (TYPEOF(v)) {
    case LGLSXP:
	for (i = 0; i < n; i++) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = Rf_ComplexFromLogical(LOGICAL_ELT(v, i), &warn);
	}
	break;
    case INTSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = Rf_ComplexFromInteger(INTEGER_ELT(v, i), &warn);
	}
	break;
    case REALSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = Rf_ComplexFromReal(REAL_ELT(v, i), &warn);
	}
	break;
    case STRSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = Rf_ComplexFromString(STRING_ELT(v, i), &warn);
	}
	break;
    case RAWSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = Rf_ComplexFromInteger(int(RAW_ELT(v, i)), &warn);
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("coerceToComplex", v);
    }
    if (warn) Rf_CoercionWarning(warn);
    UNPROTECT(1);
    return ans;
}

static SEXP coerceToRaw(SEXP v)
{
    SEXP ans;
    int warn = 0, tmp;
    R_xlen_t i, n;

    PROTECT(ans = Rf_allocVector(RAWSXP, n = XLENGTH(v)));
    Rbyte *pa = RAW(ans);
    ans->maybeTraceMemory(v);
    cDUPLICATE_ATTRIB(ans, v);
    switch (TYPEOF(v)) {
    case LGLSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    tmp = Rf_IntegerFromLogical(LOGICAL_ELT(v, i), &warn);
	    if(tmp == R_NaInt) {
		tmp = 0;
		warn |= WARN_RAW;
	    }
	    pa[i] = Rbyte(tmp);
	}
	break;
    case INTSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    tmp = INTEGER_ELT(v, i);
	    if(tmp == R_NaInt || tmp < 0 || tmp > 255) {
		tmp = 0;
		warn |= WARN_RAW;
	    }
	    pa[i] = Rbyte(tmp);
	}
	break;
    case REALSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    tmp = Rf_IntegerFromReal(REAL_ELT(v, i), &warn);
	    if(tmp == R_NaInt || tmp < 0 || tmp > 255) {
		tmp = 0;
		warn |= WARN_RAW;
	    }
	    pa[i] = Rbyte(tmp);
	}
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    tmp = Rf_IntegerFromComplex(COMPLEX_ELT(v, i), &warn);
	    if(tmp == R_NaInt || tmp < 0 || tmp > 255) {
		tmp = 0;
		warn |= WARN_RAW;
	    }
	    pa[i] = Rbyte(tmp);
	}
	break;
    case STRSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    tmp = Rf_IntegerFromString(STRING_ELT(v, i), &warn);
	    if(tmp == R_NaInt || tmp < 0 || tmp > 255) {
		tmp = 0;
		warn |= WARN_RAW;
	    }
	    pa[i] = Rbyte(tmp);
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("coerceToRaw", v);
    }
    if (warn) Rf_CoercionWarning(warn);
    UNPROTECT(1);
    return ans;
}

static SEXP coerceToString(SEXP v)
{
    SEXP ans;
    int savedigits, warn = 0;
    R_xlen_t i, n;

    PROTECT(ans = Rf_allocVector(STRSXP, n = XLENGTH(v)));
    ans->maybeTraceMemory(v);
    cDUPLICATE_ATTRIB(ans, v);
    switch (TYPEOF(v)) {
    case LGLSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_STRING_ELT(ans, i, Rf_StringFromLogical(LOGICAL_ELT(v, i), &warn));
	}
	break;
    case INTSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_STRING_ELT(ans, i, Rf_StringFromInteger(INTEGER_ELT(v, i), &warn));
	}
	break;
    case REALSXP:
	Rf_PrintDefaults();
	savedigits = R_print.digits; R_print.digits = DBL_DIG;/* MAX precision */
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_STRING_ELT(ans, i, Rf_StringFromReal(REAL_ELT(v, i), &warn));
	}
	R_print.digits = savedigits;
	break;
    case CPLXSXP:
	Rf_PrintDefaults();
	savedigits = R_print.digits; R_print.digits = DBL_DIG;/* MAX precision */
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_STRING_ELT(ans, i, Rf_StringFromComplex(COMPLEX_ELT(v, i), &warn));
	}
	R_print.digits = savedigits;
	break;
    case RAWSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_STRING_ELT(ans, i, StringFromRaw(RAW_ELT(v, i), &warn));
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("coerceToString", v);
    }
    if (warn) Rf_CoercionWarning(warn);/*2000/10/23*/
    UNPROTECT(1);
    return (ans);
}

static SEXP coerceToExpression(SEXP v)
{
    SEXP ans;
    R_xlen_t i, n;
    if (Rf_isVectorAtomic(v)) {
	n = XLENGTH(v);
	PROTECT(ans = Rf_allocVector(EXPRSXP, n));
	ans->maybeTraceMemory(v);
	switch (TYPEOF(v)) {
	case LGLSXP:
	    for (i = 0; i < n; i++)
		SET_XVECTOR_ELT(ans, i, Rf_ScalarLogical(LOGICAL_ELT(v, i)));
	    break;
	case INTSXP:
	    for (i = 0; i < n; i++)
		SET_XVECTOR_ELT(ans, i, Rf_ScalarInteger(INTEGER_ELT(v, i)));
	    break;
	case REALSXP:
	    for (i = 0; i < n; i++)
		SET_XVECTOR_ELT(ans, i, Rf_ScalarReal(REAL_ELT(v, i)));
	    break;
	case CPLXSXP:
	    for (i = 0; i < n; i++)
		SET_XVECTOR_ELT(ans, i, Rf_ScalarComplex(COMPLEX_ELT(v, i)));
	    break;
	case STRSXP:
	    for (i = 0; i < n; i++)
		SET_XVECTOR_ELT(ans, i, Rf_ScalarString(STRING_ELT(v, i)));
	    break;
	case RAWSXP:
	    for (i = 0; i < n; i++)
		SET_XVECTOR_ELT(ans, i, Rf_ScalarRaw(RAW_ELT(v, i)));
	    break;
	default:
	    UNIMPLEMENTED_TYPE("coerceToExpression", v);
	}
    }
    else {/* not used either */
	PROTECT(ans = Rf_allocVector(EXPRSXP, 1));
	SET_XVECTOR_ELT(ans, 0, Rf_duplicate(v));
    }
    UNPROTECT(1);
    return ans;
}

static SEXP coerceToVectorList(SEXP v)
{
    SEXP ans, tmp;
    R_xlen_t i, n;
    n = Rf_xlength(v);
    PROTECT(ans = Rf_allocVector(VECSXP, n));
    ans->maybeTraceMemory(v);
    switch (TYPEOF(v)) {
    case LGLSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_VECTOR_ELT(ans, i, Rf_ScalarLogical(LOGICAL_ELT(v, i)));
	}
	break;
    case INTSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_VECTOR_ELT(ans, i, Rf_ScalarInteger(INTEGER_ELT(v, i)));
	}
	break;
    case REALSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_VECTOR_ELT(ans, i, Rf_ScalarReal(REAL_ELT(v, i)));
	}
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_VECTOR_ELT(ans, i, Rf_ScalarComplex(COMPLEX_ELT(v, i)));
	}
	break;
    case STRSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_VECTOR_ELT(ans, i, Rf_ScalarString(STRING_ELT(v, i)));
	}
	break;
    case RAWSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_VECTOR_ELT(ans, i, Rf_ScalarRaw(RAW_ELT(v, i)));
	}
	break;
    case LISTSXP:
    case LANGSXP:
	tmp = v;
	for (i = 0; i < n; i++) {
	    SET_VECTOR_ELT(ans, i, CAR(tmp));
	    tmp = CDR(tmp);
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("coerceToVectorList", v);
    }
    tmp = Rf_getAttrib(v, Symbols::NamesSymbol);
    if (tmp != nullptr)
	Rf_setAttrib(ans, Symbols::NamesSymbol, tmp);
    UNPROTECT(1);
    return (ans);
}

static SEXP coerceToPairList(SEXP v)
{
    SEXP ans, ansp;
    int i, n;
    n = LENGTH(v); /* limited to len */
    PROTECT(ansp = ans = Rf_allocList(n));
    for (i = 0; i < n; i++) {
	switch (TYPEOF(v)) {
	case LGLSXP:
	    SETCAR(ansp, Rf_allocVector(LGLSXP, 1));
	    LOGICAL0(CAR(ansp))[0] = Rboolean(LOGICAL_ELT(v, i));
	    break;
	case INTSXP:
	    SETCAR(ansp, Rf_allocVector(INTSXP, 1));
	    INTEGER0(CAR(ansp))[0] = INTEGER_ELT(v, i);
	    break;
	case REALSXP:
	    SETCAR(ansp, Rf_allocVector(REALSXP, 1));
	    REAL0(CAR(ansp))[0] = REAL_ELT(v, i);
	    break;
	case CPLXSXP:
	    SETCAR(ansp, Rf_allocVector(CPLXSXP, 1));
	    COMPLEX0(CAR(ansp))[0] = COMPLEX_ELT(v, i);
	    break;
	case STRSXP:
	    SETCAR(ansp, Rf_ScalarString(STRING_ELT(v, i)));
	    break;
	case RAWSXP:
	    SETCAR(ansp, Rf_allocVector(RAWSXP, 1));
	    RAW0(CAR(ansp))[0] = RAW_ELT(v, i);
	    break;
	case VECSXP:
	    SETCAR(ansp, VECTOR_ELT(v, i));
	    break;
	case EXPRSXP:
	    SETCAR(ansp, VECTOR_ELT(v, i));
	    break;
	default:
	    UNIMPLEMENTED_TYPE("coerceToPairList", v);
	}
	ansp = CDR(ansp);
    }
    ansp = Rf_getAttrib(v, Symbols::NamesSymbol);
    if (ansp != nullptr)
	Rf_setAttrib(ans, Symbols::NamesSymbol, ansp);
    UNPROTECT(1);
    return (ans);
}

/* Coerce a pairlist to the given type */
static SEXP coercePairList(SEXP v, SEXPTYPE type)
{
    int i, n=0;
    SEXP rval= nullptr, vp, names;

    /* Hmm, this is also called to LANGSXP, and Rf_coerceVector already
       did the check of TYPEOF(v) == type */
    if(type == LISTSXP) return v;/* IS pairlist */

    names = v;
    if (type == EXPRSXP) {
	PROTECT(rval = Rf_allocVector(type, 1));
	SET_XVECTOR_ELT(rval, 0, v);
	UNPROTECT(1);
	return rval;
    }
    else if (type == STRSXP) {
	n = Rf_length(v);
	PROTECT(rval = Rf_allocVector(type, n));
	for (vp = v, i = 0; vp != nullptr; vp = CDR(vp), i++) {
	    if (Rf_isString(CAR(vp)) && Rf_length(CAR(vp)) == 1)
		SET_STRING_ELT(rval, i, STRING_ELT(CAR(vp), 0));
	    else
		SET_STRING_ELT(rval, i, STRING_ELT(Rf_deparse1line(CAR(vp), FALSE), 0));
	}
    }
    else if (type == VECSXP) {
	rval = Rf_PairToVectorList(v);
	return rval;
    }
    else if (Rf_isVectorizable(v)) {
	n = Rf_length(v);
	PROTECT(rval = Rf_allocVector(type, n));
	switch (type) {
	case LGLSXP:
	    for (i = 0, vp = v; i < n; i++, vp = CDR(vp))
		LOGICAL0(rval)[i] = Rboolean(Rf_asLogical(CAR(vp)));
	    break;
	case INTSXP:
	    for (i = 0, vp = v; i < n; i++, vp = CDR(vp))
		INTEGER0(rval)[i] = Rf_asInteger(CAR(vp));
	    break;
	case REALSXP:
	    for (i = 0, vp = v; i < n; i++, vp = CDR(vp))
		REAL0(rval)[i] = Rf_asReal(CAR(vp));
	    break;
	case CPLXSXP:
	    for (i = 0, vp = v; i < n; i++, vp = CDR(vp))
		COMPLEX0(rval)[i] = Rf_asComplex(CAR(vp));
	    break;
	case RAWSXP:
	    for (i = 0, vp = v; i < n; i++, vp = CDR(vp))
		RAW0(rval)[i] = Rbyte(Rf_asInteger(CAR(vp)));
	    break;
	default:
	    UNIMPLEMENTED_TYPE("coercePairList", v);
	}
    }
    else
	Rf_error(_("'pairlist' object cannot be coerced to type '%s'"),
	      Rf_type2char(type));

    /* If any tags are non-null then we */
    /* need to add a names attribute. */
    for (vp = v, i = 0; vp != nullptr; vp = CDR(vp))
	if (TAG(vp) != nullptr)
	    i = 1;

    if (i) {
	i = 0;
	names = Rf_allocVector(STRSXP, n);
	for (vp = v; vp != nullptr; vp = CDR(vp), i++)
	    if (TAG(vp) != nullptr)
		SET_STRING_ELT(names, i, PRINTNAME(TAG(vp)));
	Rf_setAttrib(rval, Symbols::NamesSymbol, names);
    }
    UNPROTECT(1);
    return rval;
}

/* Coerce a vector list to the given type */
static SEXP Rf_coerceVectorList(SEXP v, SEXPTYPE type)
{
    int warn = 0, tmp;
    R_xlen_t i, n;
    SEXP rval, names;

    names = v;
    rval = nullptr;	/* -Wall */

    /* expression -> list, new in R 2.4.0 */
    if (type == VECSXP)
	if (v->sexptype() == EXPRSXP) {
	    ExpressionVector* ev = static_cast<ExpressionVector*>(v);
	    GCStackRoot<ListVector>
		lv(ListVector::create(ev->begin(), ev->end()));
	    lv->copyAttribute(NamesSymbol, ev);
	    return lv;
	}
    if (type == EXPRSXP && TYPEOF(v) == VECSXP) {
	ListVector* lv = static_cast<ListVector*>(v);
	GCStackRoot<ExpressionVector>
	    ev(ExpressionVector::create(lv->begin(), lv->end()));
	ev->copyAttribute(NamesSymbol, lv);
	return ev;
    }

    if (type == STRSXP) {
	n = Rf_xlength(v);
	PROTECT(rval = Rf_allocVector(type, n));
	rval->maybeTraceMemory(v);
	for (i = 0; i < n;  i++) {
	    SEXP elt;
	    if (v->sexptype() == EXPRSXP) {
		ExpressionVector* ev = static_cast<ExpressionVector*>(v);
		elt = (*ev)[i];
	    }
	    else elt = VECTOR_ELT(v, i);
	    if (Rf_isString(elt) && Rf_length(elt) == 1)
		SET_STRING_ELT(rval, i, STRING_ELT(elt, 0));
#if 0
	    /* this will make as.character(list(s)) not backquote
	     * non-syntactic name s. It is not entirely clear that
	     * that is really desirable though....
	     */
	    else if (Rf_isSymbol(VECTOR_ELT(v, i)))
		SET_STRING_ELT(rval, i, PRINTNAME(VECTOR_ELT(v, i)));
#endif
	    else
		SET_STRING_ELT(rval, i,
			       STRING_ELT(Rf_deparse1line_(VECTOR_ELT(v, i), FALSE, NICE_NAMES),
					  0));
	}
    }
    else if (type == LISTSXP) {
	rval = Rf_VectorToPairList(v);
	return rval;
    }
    else if (Rf_isVectorizable(v)) {
	n = Rf_xlength(v);
	PROTECT(rval = Rf_allocVector(type, n));
	switch (type) {
	case LGLSXP:
	    for (i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		LOGICAL0(rval)[i] = Rboolean(Rf_asLogical(VECTOR_ELT(v, i)));
	    }
	    break;
	case INTSXP:
	    for (i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		INTEGER0(rval)[i] = Rf_asInteger(VECTOR_ELT(v, i));
	    }
	    break;
	case REALSXP:
	    for (i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		REAL0(rval)[i] = Rf_asReal(VECTOR_ELT(v, i));
	    }
	    break;
	case CPLXSXP:
	    for (i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		COMPLEX0(rval)[i] = Rf_asComplex(VECTOR_ELT(v, i));
	    }
	    break;
	case RAWSXP:
	    for (i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		tmp = Rf_asInteger(VECTOR_ELT(v, i));
		if (tmp < 0 || tmp > 255) { /* includes R_NaInt */
		    tmp = 0;
		    warn |= WARN_RAW;
		}
		RAW0(rval)[i] = Rbyte(tmp);
	    }
	    break;
	default:
	    UNIMPLEMENTED_TYPE("coerceVectorList", v);
	}
    }
    else
	Rf_error(_("(list) object cannot be coerced to type '%s'"),
	      Rf_type2char(type));

    if (warn) Rf_CoercionWarning(warn);
    names = Rf_getAttrib(v, Symbols::NamesSymbol);
    if (names != nullptr)
	Rf_setAttrib(rval, Symbols::NamesSymbol, names);
    UNPROTECT(1);
    return rval;
}

static SEXP coerceSymbol(SEXP v, SEXPTYPE type)
{
    SEXP rval = nullptr;
    if (type == EXPRSXP) {
	PROTECT(rval = Rf_allocVector(type, 1));
	SET_XVECTOR_ELT(rval, 0, v);
	UNPROTECT(1);
    } else if (type == CHARSXP)
	rval = PRINTNAME(v);
    else if (type == STRSXP)
	rval = Rf_ScalarString(PRINTNAME(v));
    else
	Rf_warning(_("(symbol) object cannot be coerced to type '%s'"),
		Rf_type2char(type));
    return rval;
}

SEXP Rf_coerceVector(SEXP v, SEXPTYPE type)
{
    SEXP op, vp, ans = nullptr;	/* -Wall */
    int i,n;

    if (TYPEOF(v) == type)
	return v;

    if (ALTREP(v)) {
	ans = ALTREP_COERCE(v, type);
	if (ans) return ans;
    }

    /* code to allow classes to extend ENVSXP, SYMSXP, etc */
    if(IS_S4_OBJECT(v) && TYPEOF(v) == S4SXP) {
	SEXP vv = R_getS4DataSlot(v, ANYSXP);
	if(vv == nullptr)
	  Rf_error(_("no method for coercing this S4 class to a vector"));
	else if(TYPEOF(vv) == type)
	  return vv;
	v = vv;
    }

    switch (TYPEOF(v)) {
#ifdef NOTYET
    case NILSXP:
	ans = coerceNull(v, type);
	break;
#endif
    case SYMSXP:
	ans = coerceSymbol(v, type);
	break;
    case NILSXP:
    case LISTSXP:
	ans = coercePairList(v, type);
	break;
    case LANGSXP:
	if (type != STRSXP) {
	    ans = coercePairList(v, type);
	    break;
	}

	/* This is mostly copied from coercePairList, but we need to
	 * special-case the first element so as not to get operators
	 * put in backticks. */
	n = Rf_length(v);
	PROTECT(ans = Rf_allocVector(type, n));
	if (n == 0) {
	    /* Can this actually happen? */
	    UNPROTECT(1);
	    break;
	}
	i = 0;
	op = CAR(v);
	/* The case of practical relevance is "lhs ~ rhs", which
	 * people tend to split using as.character(), modify, and
	 * paste() back together. However, we might as well
	 * special-case all symbolic operators here. */
	if (TYPEOF(op) == SYMSXP) {
	    SET_STRING_ELT(ans, i, PRINTNAME(op));
	    i++;
	    v = CDR(v);
	}

	/* The distinction between strings and other elements was
	 * here "always", but is really dubious since it makes x <- a
	 * and x <- "a" come out identical. Won't fix just now. */
	for (vp = v;  vp != nullptr; vp = CDR(vp), i++) {
	    if (Rf_isString(CAR(vp)) && Rf_length(CAR(vp)) == 1)
		SET_STRING_ELT(ans, i, STRING_ELT(CAR(vp), 0));
	    else
		SET_STRING_ELT(ans, i, STRING_ELT(Rf_deparse1line(CAR(vp), FALSE), 0));
	}
	UNPROTECT(1);
	break;
    case VECSXP:
    case EXPRSXP:
	ans = Rf_coerceVectorList(v, type);
	break;
    case ENVSXP:
	Rf_error(_("environments cannot be coerced to other types"));
	break;
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case RAWSXP:

#define COERCE_ERROR_STRING "cannot coerce type '%s' to vector of type '%s'"

#define COERCE_ERROR							\
	Rf_error(_(COERCE_ERROR_STRING), Rf_type2char(TYPEOF(v)), Rf_type2char(type))

	switch (type) {
	case SYMSXP:
	    ans = coerceToSymbol(v);	    break;
	case LGLSXP:
	    ans = coerceToLogical(v);	    break;
	case INTSXP:
	    ans = coerceToInteger(v);	    break;
	case REALSXP:
	    ans = coerceToReal(v);	    break;
	case CPLXSXP:
	    ans = coerceToComplex(v);	    break;
	case RAWSXP:
	    ans = coerceToRaw(v);	    break;
	case STRSXP:
	    if (RHO_FALSE && ATTRIB(v) == nullptr)
		switch(TYPEOF(v)) {
		case INTSXP:
		case REALSXP:
		    return R_deferred_coerceToString(v, NULL);
		default:
		break;
		}
	    ans = coerceToString(v);	    break;
	case EXPRSXP:
	    ans = coerceToExpression(v);    break;
	case VECSXP:
	    ans = coerceToVectorList(v);    break;
	case LISTSXP:
	    ans = coerceToPairList(v);	    break;
	default:
	    COERCE_ERROR;
	}
	break;
    default:
	COERCE_ERROR;
    }
    return ans;
}
#undef COERCE_ERROR


SEXP Rf_CreateTag(SEXP x)
{
    if (Rf_isNull(x) || Rf_isSymbol(x))
	return x;
    if (Rf_isString(x)
	&& Rf_length(x) >= 1
	&& Rf_length(STRING_ELT(x, 0)) >= 1) {
	x = Rf_installTrChar(STRING_ELT(x, 0));
    } else
	x = Rf_installTrChar(STRING_ELT(Rf_deparse1(x, TRUE, SIMPLEDEPARSE), 0));
    return x;
}

static SEXP asFunction(SEXP x)
{
    SEXP f;
    int n;
    if (Rf_isFunction(x)) return x;
    if (MAYBE_REFERENCED(x)) PROTECT(x = Rf_duplicate(x));
    else PROTECT(x);

    if (Rf_isNull(x) || !Rf_isList(x)) {
	f = Rf_mkCLOSXP(nullptr, x, R_GlobalEnv);
    }
    else {
	n = Rf_length(x);
	SEXP formals = Rf_allocList(n - 1);
	SEXP pf = formals;
	while(--n) {
	    if (TAG(x) == nullptr) {
		SET_TAG(pf, Rf_CreateTag(CAR(x)));
		SETCAR(pf, R_MissingArg);
	    }
	    else {
		SETCAR(pf, CAR(x));
		SET_TAG(pf, TAG(x));
	    }
	    pf = CDR(pf);
	    x = CDR(x);
	}
	f = Rf_mkCLOSXP(formals, CAR(x), R_GlobalEnv);
    }
    UNPROTECT(1);
    return f;
}

static SEXP ascommon(SEXP call, SEXP u, SEXPTYPE type)
{
    /* -> as.vector(..) or as.XXX(.) : coerce 'u' to 'type' : */
    /* code assumes u is protected */

    SEXP v;
    if (type == CLOSXP) {
	return asFunction(u);
    }
    else if (Rf_isVector(u) || Rf_isList(u) || Rf_isLanguage(u)
	     || (Rf_isSymbol(u) && type == EXPRSXP)) {
	if (type != ANYSXP && TYPEOF(u) != type) v = Rf_coerceVector(u, type);
	else v = u;

	/* drop attributes() and class() in some cases for as.pairlist:
	   But why?  (And who actually coerces to pairlists?)
	 */
	if ((type == LISTSXP) &&
	    !(TYPEOF(u) == LANGSXP || TYPEOF(u) == LISTSXP ||
	      TYPEOF(u) == EXPRSXP || TYPEOF(u) == VECSXP)) {
	    if (MAYBE_REFERENCED(v)) v = Rf_shallow_duplicate(v);
	    CLEAR_ATTRIB(v);
	}
	return v;
    }
    else if (Rf_isSymbol(u) && type == STRSXP)
	return Rf_ScalarString(PRINTNAME(u));
    else if (Rf_isSymbol(u) && type == SYMSXP)
	return u;
    else if (Rf_isSymbol(u) && type == VECSXP) {
	v = Rf_allocVector(VECSXP, 1);
	SET_VECTOR_ELT(v, 0, u);
	return v;
    }
    else Rf_errorcall(call, _(COERCE_ERROR_STRING),
		   Rf_type2char(TYPEOF(u)), Rf_type2char(type));
    return u;/* -Wall */
}

HIDDEN SEXP do_asCharacterFactor(Expression* call, const BuiltInFunction* op, RObject* x)
{
    return Rf_asCharacterFactor(x);
}

/* used in attrib.cpp, eval.cpp and unique.cpp */
SEXP Rf_asCharacterFactor(SEXP x)
{
    SEXP ans;

    if( !inherits2(x, "factor") )
	Rf_error(_("attempting to coerce non-factor"));

    R_xlen_t i, n = XLENGTH(x);
    SEXP labels = Rf_getAttrib(x, Symbols::LevelsSymbol);
    if (TYPEOF(labels) != STRSXP)
	Rf_error(_("malformed factor"));
    int nl = LENGTH(labels);
    PROTECT(ans = Rf_allocVector(STRSXP, n));
    for(i = 0; i < n; i++) {
      int ii = INTEGER_ELT(x, i);
      if (ii == R_NaInt)
	  SET_STRING_ELT(ans, i, R_NaString);
      else if (ii >= 1 && ii <= nl)
	  SET_STRING_ELT(ans, i, STRING_ELT(labels, ii - 1));
      else
	  Rf_error(_("malformed factor"));
    }
    UNPROTECT(1);
    return ans;
}


HIDDEN SEXP do_asatomic(/*const*/ Expression* call, const BuiltInFunction* op, int num_args, ...)
{
    SEXP ans, x;

    SEXPTYPE type = STRSXP;
    switch(op->variant()) {
    case 0:
	type = STRSXP; break;
    case 1:
	type = INTSXP; break;
    case 2:
	type = REALSXP; break;
    case 3:
	type = CPLXSXP; break;
    case 4:
	type = LGLSXP; break;
    case 5:
	type = RAWSXP; break;
    }

    if (type == RAWSXP) {
	op->checkNumArgs(num_args, 1, call);
    }

    x = nullptr;
    if (num_args) {
	UNPACK_VA_ARGS(num_args, (y));
	x = y;
    }
    if(TYPEOF(x) == type) {
	if(ATTRIB(x) == nullptr) return x;
	ans = MAYBE_REFERENCED(x) ? Rf_duplicate(x) : x;
	CLEAR_ATTRIB(ans);
	return ans;
    }
    ans = ascommon(call, x, type);
    CLEAR_ATTRIB(ans);
    return ans;
}

/* NB: as.vector is used for several other as.xxxx, including
   as.expression, as.list, as.pairlist, as.symbol, (as.single) */
HIDDEN SEXP do_asvector(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x, RObject* mode)
{
    SEXP ans;
    SEXPTYPE type;

    if (!Rf_isString(mode) || LENGTH(mode) != 1)
	error_return(R_MSG_mode);
    if (streql("function", (R_CHAR(STRING_ELT(mode, 0))))) /* ASCII */
	type = CLOSXP;
    else
	type = Rf_str2type(R_CHAR(STRING_ELT(mode, 0))); /* ASCII */

    /* "any" case added in 2.13.0 */
    if(type == ANYSXP || TYPEOF(x) == type) {
	switch(TYPEOF(x)) {
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	case RAWSXP:
	    if(ATTRIB(x) == nullptr) return x;
	    ans  = MAYBE_REFERENCED(x) ? Rf_duplicate(x) : x;
	    CLEAR_ATTRIB(ans);
	    return ans;
	case EXPRSXP:
	case VECSXP:
	    return x;
	default:
	    ;
	}
    }

    if(IS_S4_OBJECT(x) && TYPEOF(x) == S4SXP) {
	SEXP v = R_getS4DataSlot(x, ANYSXP);
	if(v == nullptr)
	    Rf_error(_("no method for coercing this S4 class to a vector"));
	x = v;
    }

    switch(type) {/* only those are valid : */
    case SYMSXP: /* for as.symbol */
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case EXPRSXP: /* for as.expression */
    case VECSXP: /* list */
    case LISTSXP:/* for as.pairlist */
    case CLOSXP: /* non-primitive function */
    case RAWSXP:
    case ANYSXP: /* any */
	break;
    default:
	error_return(R_MSG_mode);
    }
    ans = ascommon(call, x, type);
    switch(TYPEOF(ans)) { /* keep attributes for these: */
    case NILSXP: /* doesn't have any */
    case LISTSXP: /* but ascommon fiddled */
    case LANGSXP:
    case VECSXP:
    case EXPRSXP:
	break;
    default:
	CLEAR_ATTRIB(ans);
	break;
    }
    return ans;
}


HIDDEN SEXP do_asfunction(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* envir_)
{
    SEXP arglist, envir, names, args, pargs, body;
    int i, n;

    /* Check the arguments; we need a list and environment. */

    arglist = x_;
    if (!Rf_isNewList(arglist))
	Rf_error(_("list argument expected"));

    envir = downcast_to_env(envir_);
    if (!envir)
	Rf_error(_("invalid environment"));

    n = Rf_length(arglist);
    if (n < 1)
	Rf_error(_("argument must have length at least 1"));
    names = Rf_getAttrib(arglist, Symbols::NamesSymbol);
    PROTECT(pargs = args = Rf_allocList(n - 1));
    for (i = 0; i < n - 1; i++) {
	SETCAR(pargs, VECTOR_ELT(arglist, i));
	if (names != nullptr && *R_CHAR(STRING_ELT(names, i)) != '\0') /* ASCII */
	    SET_TAG(pargs, Rf_installTrChar(STRING_ELT(names, i)));
	else
	    SET_TAG(pargs, nullptr);
	pargs = CDR(pargs);
    }
    PROTECT(body = VECTOR_ELT(arglist, n-1));
    /* the main (only?) thing to rule out is body being
       a function already. If we test here then
       Rf_mkCLOSXP can continue to overreact when its
       test fails (PR#1880, 7535, 7702) */
    if(Rf_isList(body) || Rf_isLanguage(body) || Rf_isSymbol(body)
       || Rf_isExpression(body) || Rf_isVector(body) || isByteCode(body)
       )
	    args =  Rf_mkCLOSXP(args, body, envir);
    else
	    Rf_error(_("invalid body for function"));
    UNPROTECT(2);
    return args;
}


/* primitive */
HIDDEN SEXP do_ascall(/*const*/ Expression* call, const BuiltInFunction* op, RObject* args)
{
    SEXP ap, ans, names;
    int i, n;

    switch (TYPEOF(args)) {
    case LANGSXP:
	ans = args;
	break;
    case VECSXP:
	{
	    if(0 == (n = Rf_length(args)))
		Rf_errorcall(call, _("invalid length 0 argument"));
	    names = Rf_getAttrib(args, Symbols::NamesSymbol);
	    GCStackRoot<PairList> tl(PairList::make(n - 1));
	    PROTECT(ap = ans = new CachingExpression(nullptr, tl));
	    for (i = 0; i < n; i++) {
		SETCAR(ap, VECTOR_ELT(args, i));
		if (names != nullptr && !Rf_StringBlank(STRING_ELT(names, i)))
		    SET_TAG(ap, Rf_install(Rf_translateChar(STRING_ELT(names, i))));
		ap = CDR(ap);
	    }
	    UNPROTECT(1);
	    break;
	}
    case EXPRSXP:
	{
	    if(0 == (n = Rf_length(args)))
		Rf_errorcall(call, _("invalid length 0 argument"));
	    names = Rf_getAttrib(args, Symbols::NamesSymbol);
	    GCStackRoot<PairList> tl(PairList::make(n - 1));
	    PROTECT(ap = ans = new CachingExpression(nullptr, tl));
	    for (i = 0; i < n; i++) {
		SETCAR(ap, XVECTOR_ELT(args, i));
		if (names != nullptr && !Rf_StringBlank(STRING_ELT(names, i)))
		    SET_TAG(ap, Rf_installTrChar(STRING_ELT(names, i)));
		ap = CDR(ap);
	    }
	    UNPROTECT(1);
	    break;
	}
    case LISTSXP:
	{
	    ConsCell* cc = SEXP_downcast<ConsCell*>(args);
	    GCStackRoot<Expression> ansr(ConsCell::convert<Expression>(cc));
	    ans = ansr;
	    break;
	}
    case STRSXP:
	Rf_errorcall(call, _("as.call(<character string>)  not yet implemented"));
	break;
    default:
	Rf_errorcall(call, _("invalid argument list"));
	ans = nullptr;
    }
    SET_TAG(ans, nullptr);
    return ans;
}


/* int, not Rboolean, for R_NaLog : */
int Rf_asLogical(SEXP x)
{
    int warn = 0;

    if (Rf_isVectorAtomic(x)) {
	if (XLENGTH(x) < 1)
	    return R_NaLog;
	switch (TYPEOF(x)) {
	case LGLSXP:
	    return LOGICAL_ELT(x, 0);
	case INTSXP:
	    return Rf_LogicalFromInteger(INTEGER_ELT(x, 0), &warn);
	case REALSXP:
	    return Rf_LogicalFromReal(REAL_ELT(x, 0), &warn);
	case CPLXSXP:
	    return Rf_LogicalFromComplex(COMPLEX_ELT(x, 0), &warn);
	case STRSXP:
	    return Rf_LogicalFromString(STRING_ELT(x, 0), &warn);
	case RAWSXP:
	    return Rf_LogicalFromInteger((int)RAW_ELT(x, 0), &warn);
	default:
	    UNIMPLEMENTED_TYPE("asLogical", x);
	}
    } else if(TYPEOF(x) == CHARSXP) {
	    return Rf_LogicalFromString(x, &warn);
    }
    return R_NaLog;
}

int Rf_asInteger(SEXP x)
{
    int warn = 0, res;

    if (Rf_isVectorAtomic(x) && XLENGTH(x) >= 1) {
	switch (TYPEOF(x)) {
	case LGLSXP:
	    return Rf_IntegerFromLogical(LOGICAL_ELT(x, 0), &warn);
	case INTSXP:
	    return INTEGER_ELT(x, 0);
	case REALSXP:
	    res = Rf_IntegerFromReal(REAL_ELT(x, 0), &warn);
	    Rf_CoercionWarning(warn);
	    return res;
	case CPLXSXP:
	    res = Rf_IntegerFromComplex(COMPLEX_ELT(x, 0), &warn);
	    Rf_CoercionWarning(warn);
	    return res;
	case STRSXP:
	    res = Rf_IntegerFromString(STRING_ELT(x, 0), &warn);
	    Rf_CoercionWarning(warn);
	    return res;
	default:
	    UNIMPLEMENTED_TYPE("asInteger", x);
	}
    } else if(TYPEOF(x) == CHARSXP) {
	res = Rf_IntegerFromString(x, &warn);
	Rf_CoercionWarning(warn);
	return res;
    }
    return R_NaInt;
}

double Rf_asReal(SEXP x)
{
    int warn = 0;
    double res;

    if (Rf_isVectorAtomic(x) && XLENGTH(x) >= 1) {
	switch (TYPEOF(x)) {
	case LGLSXP:
	    res = Rf_RealFromLogical(LOGICAL_ELT(x, 0), &warn);
	    Rf_CoercionWarning(warn);
	    return res;
	case INTSXP:
	    res = Rf_RealFromInteger(INTEGER_ELT(x, 0), &warn);
	    Rf_CoercionWarning(warn);
	    return res;
	case REALSXP:
	    return REAL_ELT(x, 0);
	case CPLXSXP:
	    res = Rf_RealFromComplex(COMPLEX_ELT(x, 0), &warn);
	    Rf_CoercionWarning(warn);
	    return res;
	case STRSXP:
	    res = Rf_RealFromString(STRING_ELT(x, 0), &warn);
	    Rf_CoercionWarning(warn);
	    return res;
	default:
	    UNIMPLEMENTED_TYPE("asReal", x);
	}
    } else if(TYPEOF(x) == CHARSXP) {
	res = Rf_RealFromString(x, &warn);
	Rf_CoercionWarning(warn);
	return res;
    }
    return R_NaReal;
}

Rcomplex Rf_asComplex(SEXP x)
{
    int warn = 0;
    Rcomplex z;

    if (Rf_isVectorAtomic(x) && XLENGTH(x) >= 1) {
	switch (TYPEOF(x)) {
	case LGLSXP:
	    z = Rf_ComplexFromLogical(LOGICAL_ELT(x, 0), &warn);
	    Rf_CoercionWarning(warn);
	    return z;
	case INTSXP:
	    z = Rf_ComplexFromInteger(INTEGER_ELT(x, 0), &warn);
	    Rf_CoercionWarning(warn);
	    return z;
	case REALSXP:
	    z = Rf_ComplexFromReal(REAL_ELT(x, 0), &warn);
	    Rf_CoercionWarning(warn);
	    return z;
	case CPLXSXP:
	    return COMPLEX_ELT(x, 0);
	case STRSXP:
	    z = Rf_ComplexFromString(STRING_ELT(x, 0), &warn);
	    Rf_CoercionWarning(warn);
	    return z;
	default:
	    UNIMPLEMENTED_TYPE("asComplex", x);
	}
    } else if(TYPEOF(x) == CHARSXP) {
	z = Rf_ComplexFromString(x, &warn);
	Rf_CoercionWarning(warn);
	return z;
    }
    z.r = R_NaReal;
    z.i = R_NaReal;
    return z;
}


/* return the type (= "detailed mode") of the SEXP */
HIDDEN SEXP do_typeof(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_)
{
    return Rf_type2rstr(TYPEOF(x_));
}

/* Define many of the <primitive> "is.xxx" functions :
   Note that  Rf_isNull, Rf_isNumeric, etc are defined in util.cpp or ../include/Rinlinedfuns.h
*/
HIDDEN SEXP do_is(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_)
{
    SEXP ans;
    PROTECT(ans = Rf_allocVector(LGLSXP, 1));

    switch (op->variant()) {
    case NILSXP:	/* is.null */
	LOGICAL0(ans)[0] = Rf_isNull(x_);
	break;
    case LGLSXP:	/* is.logical */
	LOGICAL0(ans)[0] = Rboolean(TYPEOF(x_) == LGLSXP);
	break;
    case INTSXP:	/* is.integer */
	LOGICAL0(ans)[0] = Rboolean((TYPEOF(x_) == INTSXP)
	    && !Rf_inherits(x_, "factor"));
	break;
    case REALSXP:	/* is.double */
	LOGICAL0(ans)[0] = Rboolean(TYPEOF(x_) == REALSXP);
	break;
    case CPLXSXP:	/* is.complex */
	LOGICAL0(ans)[0] = Rboolean(TYPEOF(x_) == CPLXSXP);
	break;
    case STRSXP:	/* is.character */
	LOGICAL0(ans)[0] = Rboolean(TYPEOF(x_) == STRSXP);
	break;
    case SYMSXP:	/* is.symbol === is.name */
	if(IS_S4_OBJECT(x_) && (TYPEOF(x_) == S4SXP)) {
	    SEXP dot_xData = R_getS4DataSlot(x_, SYMSXP);
	    LOGICAL0(ans)[0] = Rboolean(TYPEOF(dot_xData) == SYMSXP);
	}
	else
	    LOGICAL0(ans)[0] = Rboolean(TYPEOF(x_) == SYMSXP);
	break;
    case ENVSXP:	/* is.environment */
	if(IS_S4_OBJECT(x_) && (TYPEOF(x_) == S4SXP)) {
	    SEXP dot_xData = R_getS4DataSlot(x_, ENVSXP);
	    LOGICAL0(ans)[0] = Rboolean(TYPEOF(dot_xData) == ENVSXP);
	}
	else
	    LOGICAL0(ans)[0] = Rboolean(TYPEOF(x_) == ENVSXP);
	break;
    case VECSXP:	/* is.list */
	LOGICAL0(ans)[0] = Rboolean(TYPEOF(x_) == VECSXP ||
			    TYPEOF(x_) == LISTSXP);
	break;
    case LISTSXP:	/* is.pairlist */
	LOGICAL0(ans)[0] = Rboolean(TYPEOF(x_) == LISTSXP ||
			    TYPEOF(x_) == NILSXP);/* pairlist() -> NULL */
	break;
    case EXPRSXP:	/* is.expression */
	LOGICAL0(ans)[0] = Rboolean(TYPEOF(x_) == EXPRSXP);
	break;
    case RAWSXP:	/* is.raw */
	LOGICAL0(ans)[0] = Rboolean(TYPEOF(x_) == RAWSXP);
	break;

    case 50:		/* is.object */
	LOGICAL0(ans)[0] = OBJECT(x_);
	break;
    case 51:		/* isS4 */
	LOGICAL0(ans)[0] = Rboolean(IS_S4_OBJECT(x_) != 0);
	break;
/* no longer used: is.data.frame is R code
    case 80:
	LOGICAL0(ans)[0] = Rf_isFrame(x_);
	break;
*/

    case 100:		/* is.numeric */
	LOGICAL0(ans)[0] = Rboolean(Rf_isNumeric(x_) &&
	    !Rf_isLogical(x_));  /* isNumeric excludes factors */
	break;
    case 101:		/* is.matrix */
	LOGICAL0(ans)[0] = Rf_isMatrix(x_);
	break;
    case 102:		/* is.array */
	LOGICAL0(ans)[0] = Rf_isArray(x_);
	break;

    case 200:		/* is.atomic */
	switch(TYPEOF(x_)) {
	case NILSXP:
	    /* NULL is atomic (S compatibly), but not in Rf_isVectorAtomic(.) */
	case CHARSXP:
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	case RAWSXP:
	    LOGICAL0(ans)[0] = Rboolean(1);
	    break;
	default:
	    LOGICAL0(ans)[0] = Rboolean(0);
	    break;
	}
	break;
    case 201:		/* is.recursive */
	switch(TYPEOF(x_)) {
	case VECSXP:
	case LISTSXP:
	case CLOSXP:
	case ENVSXP:
	case PROMSXP:
	case LANGSXP:
	case SPECIALSXP:
	case BUILTINSXP:
	case DOTSXP:
	case ANYSXP:
	case EXPRSXP:
	// Not recursive, as long as not subsettable (on the R level)
	// case EXTPTRSXP:
	// case BCODESXP:
	// case WEAKREFSXP:
	    LOGICAL0(ans)[0] = Rboolean(1);
	    break;
	default:
	    LOGICAL0(ans)[0] = Rboolean(0);
	    break;
	}
	break;

    case 300:		/* is.call */
	LOGICAL0(ans)[0] = Rboolean(TYPEOF(x_) == LANGSXP);
	break;
    case 301:		/* is.language */
	LOGICAL0(ans)[0] = Rboolean(TYPEOF(x_) == SYMSXP ||
			    TYPEOF(x_) == LANGSXP ||
			    TYPEOF(x_) == EXPRSXP);
	break;
    case 302:		/* is.function */
	LOGICAL0(ans)[0] = Rf_isFunction(x_);
	break;

    case 999:		/* is.single */
	Rf_errorcall(call, _("type \"single\" unimplemented in R"));
    default:
	Rf_errorcall(call, _("unimplemented predicate"));
    }
    UNPROTECT(1);
    return (ans);
}

/* What should is.vector do ?
 * In S, if an object has no attributes it is a vector, otherwise it isn't.
 * It seems to make more sense to check for a dim attribute.
 */

// is.vector(x, mode) :
HIDDEN SEXP do_isvector(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* mode_)
{
    SEXP ans, a, x;
    const char *stype;

    x = x_;
    if (!Rf_isString(mode_) || LENGTH(mode_) != 1)
	error_return(R_MSG_mode);

    stype = R_CHAR(STRING_ELT(mode_, 0)); /* ASCII */

    /* "name" and "symbol" are synonymous */
    if (streql(stype, "name"))
      stype = "symbol";

    PROTECT(ans = Rf_allocVector(LGLSXP, 1));
    if (streql(stype, "any")) {
	/* isVector is inlined, means atomic or VECSXP or EXPRSXP */
	LOGICAL0(ans)[0] = Rf_isVector(x);
    }
    else if (streql(stype, "numeric")) {
	LOGICAL0(ans)[0] = Rboolean(Rf_isNumeric(x) && !Rf_isLogical(x));
    }
    /* So this allows any type, including undocumented ones such as
       "closure", but not aliases such as "name" and "function". */
    else if (streql(stype, Rf_type2char(TYPEOF(x)))) {
	LOGICAL0(ans)[0] = Rboolean(1);
    }
    else
	LOGICAL0(ans)[0] = Rboolean(0);

    /* We allow a "names" attribute on any vector. */
    if (LOGICAL0(ans)[0] && ATTRIB(x_) != nullptr) {
	a = ATTRIB(x_);
	while(a != nullptr) {
	    if (TAG(a) != Symbols::NamesSymbol) {
		LOGICAL0(ans)[0] = Rboolean(0);
		break;
	    }
	    a = CDR(a);
	}
    }
    UNPROTECT(1);
    return (ans);
}

namespace {
    inline int LIST_VEC_NA(SEXP s)
    {
	if (!Rf_isVector(s) || Rf_length(s) != 1)
	    return 0;
	else {
	    switch (TYPEOF(s)) {
	    case LGLSXP:
	    case INTSXP:
		return (INTEGER(s)[0] == R_NaInt);
		break;
	    case REALSXP:
		return std::isnan(REAL(s)[0]);
		break;
	    case STRSXP:
		return (STRING_ELT(s, 0) == R_NaString);
		break;
	    case CPLXSXP:
		return (std::isnan(COMPLEX(s)[0].r)
			|| std::isnan(COMPLEX(s)[0].i));
		break;
	    default:
		return 0;
	    }
	}
    }
}

R_INLINE static void copyDimAndNames(SEXP x, SEXP ans)
{
    if (Rf_isVector(x)) {
	/* PROTECT/UNPROTECT are probably not needed here */
	SEXP dims, names;
	PROTECT(dims = Rf_getAttrib(x, Symbols::DimSymbol));
	if (dims != nullptr)
	    Rf_setAttrib(ans, Symbols::DimSymbol, dims);
	UNPROTECT(1);
	if (Rf_isArray(x)) {
	    PROTECT(names = Rf_getAttrib(x, Symbols::DimNamesSymbol));
	    if (names != nullptr)
		Rf_setAttrib(ans, Symbols::DimNamesSymbol, names);
	    UNPROTECT(1);
	}
	else {
	    PROTECT(names = Rf_getAttrib(x, Symbols::NamesSymbol));
	    if (names != nullptr)
		Rf_setAttrib(ans, Symbols::NamesSymbol, names);
	    UNPROTECT(1);
	}
    }
}

HIDDEN SEXP do_isna(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_)
{
    SEXP ans, x;
    R_xlen_t i, n;

#ifdef stringent_is
    if (!Rf_isList(args[0]) && !Rf_isVector(args[0])))
	errorcall_return(call, "is.na " R_MSG_list_vec);

#endif
    x = x_;
    n = Rf_xlength(x);
    PROTECT(ans = Rf_allocVector(LGLSXP, n));
    int *pa = LOGICAL(ans);
    switch (TYPEOF(x)) {
    case LGLSXP:
       for (i = 0; i < n; i++)
	   pa[i] = (LOGICAL_ELT(x, i) == R_NaLog);
	break;
    case INTSXP:
	for (i = 0; i < n; i++)
	    pa[i] = (INTEGER_ELT(x, i) == R_NaInt);
	break;
    case REALSXP:
	for (i = 0; i < n; i++)
	    pa[i] = std::isnan(REAL_ELT(x, i));
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++) {
	    Rcomplex v = COMPLEX_ELT(x, i);
	    pa[i] = (std::isnan(v.r) || std::isnan(v.i));
	}
	break;
    case STRSXP:
	for (i = 0; i < n; i++)
	    pa[i] = (STRING_ELT(x, i) == R_NaString);
	break;
    case LISTSXP:
	for (i = 0; i < n; i++) {
	    LOGICAL(ans)[i] = LIST_VEC_NA(CAR(x));
	    x = CDR(x);
	}
	break;
    case VECSXP:
	for (i = 0; i < n; i++) {
	    SEXP s = VECTOR_ELT(x, i);
	    LOGICAL(ans)[i] = LIST_VEC_NA(s);
	}
	break;
    case RAWSXP:
	/* no such thing as a raw NA */
	for (i = 0; i < n; i++)
	    pa[i] = 0;
	break;
    case NILSXP: break;
    default:
	Rf_warningcall(call, _("%s() applied to non-(list or vector) of type '%s'"),
		    "is.na", Rf_type2char(TYPEOF(x)));
	for (i = 0; i < n; i++)
	    pa[i] = 0;
    }

    copyDimAndNames(x, ans);
    UNPROTECT(2); /* args, ans */
    return ans;
}

#include <R_ext/Itermacros.h>

// Check if x has missing values; the anyNA.default() method
static Rboolean anyNA(const Expression* call, const BuiltInFunction* op,
                      SEXP x, bool recursive, Environment* env)
/* Original code:
   Copyright 2012 Google Inc. All Rights Reserved.
   Author: Tim Hesterberg <rocket@google.com>
   Distributed under GPL 2 or later
*/
{
    SEXPTYPE xT = TYPEOF(x);
    Rboolean isList = Rboolean(xT == VECSXP || xT == LISTSXP);

    if (OBJECT(x) || (isList && !recursive)) {
	SEXP e0 = PROTECT(Rf_lang2(Rf_install("is.na"), x));
	SEXP e = PROTECT(Rf_lang2(Rf_install("any"), e0));
	SEXP res = PROTECT(Rf_eval(e, env));
	int ans = Rf_asLogical(res);
	UNPROTECT(3);
	return Rboolean(ans == 1); // so NA answer is false.
    }

    R_xlen_t i, n = Rf_xlength(x);
    switch (xT) {
    case REALSXP:
    {
	if(REAL_NO_NA(x))
	    return FALSE;
	ITERATE_BY_REGION(x, xD, i, nbatch, double, REAL, {
		for (int k = 0; k < nbatch; k++)
		    if (std::isnan(xD[k]))
			return TRUE;
	    });
	break;
    }
    case INTSXP:
    {
	if(INTEGER_NO_NA(x))
	    return FALSE;
	ITERATE_BY_REGION(x, xI, i, nbatch, int, INTEGER, {
		for (int k = 0; k < nbatch; k++)
		    if (xI[k] == R_NaInt)
			return TRUE;
	    });
	break;
    }
    case LGLSXP:
    {
	for (i = 0; i < n; i++)
	    if (LOGICAL_ELT(x, i) == R_NaLog) return TRUE;
	break;
    }
    case CPLXSXP:
    {
	for (i = 0; i < n; i++) {
	    Rcomplex v = COMPLEX_ELT(x, i);
	    if (std::isnan(v.r) || std::isnan(v.i)) return TRUE;
	}
	break;
    }
    case STRSXP:
	for (i = 0; i < n; i++)
	    if (STRING_ELT(x, i) == R_NaString) return TRUE;
	break;
    case RAWSXP: /* no such thing as a raw NA:  is.na(.) gives FALSE always */
	return FALSE;
    case NILSXP: // is.na() gives a warning..., but we do not.
	return FALSE;
    // The next two cases are only used if recursive = TRUE
    case LISTSXP:
    {
        ArgList args2({ x, Rf_ScalarLogical(recursive) },
                      ArgList::EVALUATED);
	Expression* call2 = new Expression(call->car(), args2);
	for (i = 0; i < n; i++, x = CDR(x)) {
	    SETCADR(call2, CAR(x));
            args2.set(i, CAR(x));
            auto dispatched = Rf_Dispatch(call2, op, args2, env);
            if (dispatched.first) {
                if (Rf_asLogical(dispatched.second)) {
                    return TRUE;
                }
            } else if (anyNA(call2, op, CAR(x), recursive, env)) {
                    return TRUE;
	    }
	}
	break;
    }
    case VECSXP:
    {
        ArgList args2({ x, Rf_ScalarLogical(recursive) },
                      ArgList::EVALUATED);
	Expression* call2 = new Expression(call->car(), args2);
	for (i = 0; i < n; i++) {
            args2.set(0, VECTOR_ELT(x, i));
            SETCADR(call2, VECTOR_ELT(x, i));
            auto dispatched = Rf_Dispatch(call2, op, args2, env);
            if (dispatched.first) {
                if (Rf_asLogical(dispatched.second)) {
                    return TRUE;
                }
            } else if (anyNA(call2, op, VECTOR_ELT(x, i), recursive, env)) {
                    return TRUE;
	    }
	}
	break;
    }

    default:
	Rf_error("anyNA() applied to non-(list or vector) of type '%s'",
		 Rf_type2char(TYPEOF(x)));
    }
    return FALSE;
} // anyNA()

HIDDEN SEXP do_anyNA(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    const Expression* callx = SEXP_downcast<const Expression*>(call);
    const BuiltInFunction* func = SEXP_downcast<const BuiltInFunction*>(op);
    Environment* env = SEXP_downcast<Environment*>(rho);

    ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::EVALUATED);
    if (arglist.size() < 1 || arglist.size() > 2)
	Rf_errorcall(call, "anyNA takes 1 or 2 arguments");

    auto dispatched = Rf_Dispatch(callx,
                                  SEXP_downcast<const BuiltInFunction*>(op),
                                  arglist,
                                  SEXP_downcast<Environment*>(rho));
    if (dispatched.first)
        return dispatched.second;

    if (arglist.size() == 1) {
	callx->check1arg("x");
	bool recursive = false;
 	return Rf_ScalarLogical(anyNA(callx, func, arglist.get(0), recursive,
                                      env));
   } else {
	/* This is a primitive, so we manage argument matching ourselves.
	   But this takes a little time.
	 */
	static GCRoot<ArgMatcher> matcher
	    = new ArgMatcher({ "x", "recursive" });
	SEXP x, recursive_;
	matcher->match(arglist, { &x, &recursive_ });
	bool recursive = recursive_ == R_MissingArg
	    ? false : Rf_asLogical(recursive_);
	return Rf_ScalarLogical(anyNA(callx, func, x, recursive, env));
    }
}


HIDDEN SEXP do_isnan(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_)
{
    SEXP ans, x;
    R_xlen_t i, n;

#ifdef stringent_is
    if (!Rf_isList(args[0]) && !Rf_isVector(args[0]))
	errorcall_return(call, "is.nan " R_MSG_list_vec);
#endif
    x = x_;
    n = Rf_xlength(x);
    PROTECT(ans = Rf_allocVector(LGLSXP, n));
    int *pa = LOGICAL(ans);
    switch (TYPEOF(x)) {
    case STRSXP:
    case RAWSXP:
    case NILSXP:
    case LGLSXP:
    case INTSXP:
	for (i = 0; i < n; i++)
	    pa[i] = 0;
	break;
    case REALSXP:
	for (i = 0; i < n; i++)
	    pa[i] = R_IsNaN(REAL_ELT(x, i));
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++) {
	    Rcomplex v = COMPLEX_ELT(x, i);
	    pa[i] = (R_IsNaN(v.r) || R_IsNaN(v.i));
	}
	break;
    default:
	Rf_errorcall(call, _("default method not implemented for type '%s'"),
		     Rf_type2char(TYPEOF(x)));
    }
    copyDimAndNames(x, ans);
    UNPROTECT(2); /* args, ans*/
    return ans;
}

HIDDEN SEXP do_isfinite(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_)
{
    SEXP ans, x, names, dims;
    R_xlen_t i, n;

#ifdef stringent_is
    if (!Rf_isList(CAR(args)) && !Rf_isVector(CAR(args)))
	errorcall_return(call, "is.finite " R_MSG_list_vec);
#endif
    x = x_;
    n = Rf_xlength(x);
    ans = Rf_allocVector(LGLSXP, n);
    int *pa = LOGICAL(ans);
    if (Rf_isVector(x)) {
	dims = Rf_getAttrib(x, Symbols::DimSymbol);
	if (Rf_isArray(x))
	    names = Rf_getAttrib(x, Symbols::DimNamesSymbol);
	else
	    names = Rf_getAttrib(x, Symbols::NamesSymbol);
    }
    else dims = names = nullptr;
    switch (TYPEOF(x)) {
    case STRSXP:
    case RAWSXP:
    case NILSXP:
	for (i = 0; i < n; i++)
	    pa[i] = 0;
	break;
    case LGLSXP:
    case INTSXP:
	for (i = 0; i < n; i++)
	    pa[i] = (INTEGER_ELT(x, i) != R_NaInt);
	break;
    case REALSXP:
	for (i = 0; i < n; i++)
	    pa[i] = std::isfinite(REAL_ELT(x, i));
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++) {
	    Rcomplex v = COMPLEX_ELT(x, i);
	    pa[i] = (std::isfinite(v.r) && std::isfinite(v.i));
	}
	break;
    default:
	Rf_errorcall(call, _("default method not implemented for type '%s'"),
		     Rf_type2char(TYPEOF(x)));
    }
    if (dims != nullptr)
	Rf_setAttrib(ans, Symbols::DimSymbol, dims);
    if (names != nullptr) {
	if (Rf_isArray(x))
	    Rf_setAttrib(ans, Symbols::DimNamesSymbol, names);
	else
	    Rf_setAttrib(ans, Symbols::NamesSymbol, names);
    }
    return ans;
}

HIDDEN SEXP do_isinfinite(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_)
{
    SEXP ans, x, names, dims;
    double xr, xi;
    R_xlen_t i, n;

#ifdef stringent_is
    if (!Rf_isList(CAR(args)) && !Rf_isVector(CAR(args)))
	errorcall_return(call, "is.infinite " R_MSG_list_vec);
#endif
    x = x_;
    n = Rf_xlength(x);
    ans = Rf_allocVector(LGLSXP, n);
    int *pa = LOGICAL(ans);
    if (Rf_isVector(x)) {
	dims = Rf_getAttrib(x, Symbols::DimSymbol);
	if (Rf_isArray(x))
	    names = Rf_getAttrib(x, Symbols::DimNamesSymbol);
	else
	    names = Rf_getAttrib(x, Symbols::NamesSymbol);
    }
    else	dims = names = nullptr;
    switch (TYPEOF(x)) {
    case STRSXP:
    case RAWSXP:
    case NILSXP:
    case LGLSXP:
    case INTSXP:
	for (i = 0; i < n; i++)
	    pa[i] = 0;
	break;
    case REALSXP:
	for (i = 0; i < n; i++) {
	    xr = REAL_ELT(x, i);
	    if (std::isnan(xr) || std::isfinite(xr))
		pa[i] = 0;
	    else
		pa[i] = 1;
	}
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++) {
	    Rcomplex v = COMPLEX_ELT(x, i);
	    xr = v.r;
	    xi = v.i;
	    if ((std::isnan(xr) || std::isfinite(xr)) && (std::isnan(xi) || std::isfinite(xi)))
		pa[i] = 0;
	    else
		pa[i] = 1;
	}
	break;
    default:
	Rf_errorcall(call, _("default method not implemented for type '%s'"),
		     Rf_type2char(TYPEOF(x)));
    }
    if (!Rf_isNull(dims))
	Rf_setAttrib(ans, Symbols::DimSymbol, dims);
    if (!Rf_isNull(names)) {
	if (Rf_isArray(x))
	    Rf_setAttrib(ans, Symbols::DimNamesSymbol, names);
	else
	    Rf_setAttrib(ans, Symbols::NamesSymbol, names);
    }
    return ans;
}

/* This is a primitive SPECIALSXP */
HIDDEN SEXP do_call(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rest, evargs, rfun, tmp;

    PROTECT(rfun = Rf_eval(CAR(args), rho));
    /* zero-length string check used to be here but Rf_install gives
       better error message.
     */
    if (!Rf_isString(rfun) || Rf_length(rfun) != 1)
	errorcall_return(call, _("first argument must be a character string"));
    const char *str = Rf_translateChar(STRING_ELT(rfun, 0));
    if (streql(str, ".Internal")) Rf_error("illegal usage");
    PROTECT(rfun = rho::Symbol::obtain(str));
    PROTECT(evargs = Rf_shallow_duplicate(CDR(args)));
    for (rest = evargs; rest != nullptr; rest = CDR(rest)) {
	PROTECT(tmp = Rf_eval(CAR(rest), rho));
	if (NAMED(tmp)) MARK_NOT_MUTABLE(tmp);
	SETCAR(rest, tmp);
    }
    Expression* result = new Expression(rfun, SEXP_downcast<PairList*>(evargs));
    UNPROTECT(3);
    return (result);
}

HIDDEN SEXP do_docall(/*const*/ Expression* call, const BuiltInFunction* op, RObject* what_, RObject* args_, RObject* envir_)
{
    SEXP c, fun, names, envir;
    int i, n;

    fun = what_;
    envir = envir_;
    SEXP args = args_;

    /* must be a string or a function:
       zero-length string check used to be here but Rf_install gives
       better error message.
     */
    if(!(Rf_isFunction(fun) || (Rf_isString(fun) && Rf_length(fun) == 1)))
	Rf_error(_("'what' must be a function or character string"));

#ifdef __maybe_in_the_future__
    if (!Rf_isNull(args) && !Rf_isVectorList(args))
	Rf_error(_("'args' must be a list or expression"));
#else
    if (!Rf_isNull(args) && !Rf_isNewList(args))
        Rf_error(_("'%s' must be a list"), "args");
#endif

    if (!Rf_isEnvironment(envir))
	Rf_error(_("'envir' must be an environment"));

    n = Rf_length(args);
    names = Rf_getAttrib(args, Symbols::NamesSymbol);

    GCStackRoot<PairList> tl(PairList::make(n));
    PROTECT(c = call = new Expression(nullptr, tl));
    if( Rf_isString(fun) ) {
	const char *str = Rf_translateChar(STRING_ELT(fun, 0));
	if (streql(str, ".Internal")) Rf_error("illegal usage");
	SETCAR(c, rho::Symbol::obtain(str));
    } else {
	if(TYPEOF(fun) == SPECIALSXP && streql(PRIMNAME(fun), ".Internal"))
	    Rf_error("illegal usage");
	SETCAR(c, fun);
    }
    c = CDR(c);
    for (i = 0; i < n; i++) {
#ifndef NEW
	SETCAR(c, VECTOR_ELT(args, i));
#else
	SETCAR(c, Promise::createEvaluatedPromise(VECTOR_ELT(args, i),
						  VECTOR_ELT(args, i)));
#endif
	if (Rf_ItemName(names, int(i)) != nullptr)
	    SET_TAG(c, Rf_installTrChar(Rf_ItemName(names, i)));
	c = CDR(c);
    }
    UNPROTECT(1);
    return Rf_eval(call, envir);
}


/*
   do_substitute has two arguments, an expression and an environment
   (optional).	Symbols found in the expression are substituted with their
   values as found in the environment.	There is no inheritance so only
   the supplied environment is searched. If no environment is specified
   the environment in which substitute was called is used.  If the
   specified environment is R_GlobalEnv it is converted to nullptr, for
   historical reasons. In Rf_substitute(), nullptr signals that no
   substitution should be done, only extraction of promise expressions.
   Arguments to do_substitute should not be evaluated.
*/

SEXP Rf_substitute(SEXP lang, SEXP rho)
{
    SEXP t;
    switch (TYPEOF(lang)) {
    case PROMSXP:
	return Rf_substitute(PREXPR(lang), rho);
    case SYMSXP:
	if (rho != nullptr) {
	    t = Rf_findVarInFrame3( rho, lang, TRUE);
	    if (t != R_UnboundValue) {
		if (TYPEOF(t) == PROMSXP) {
		    do {
			t = PREXPR(t);
		    } while(TYPEOF(t) == PROMSXP);
		    /* make sure code will not be modified: */
		    ENSURE_NAMEDMAX(t);
		    return t;
		}
		else if (TYPEOF(t) == DOTSXP)
		    Rf_error(_("'...' used in an incorrect context"));
		if (rho != R_GlobalEnv)
		    return t;
	    }
	}
	return (lang);
    case LANGSXP:
	return Rf_substituteList(lang, rho);
    default:
	return (lang);
    }
}


/* Work through a list doing Rf_substitute on the
   elements taking particular care to handle '...' */

HIDDEN SEXP Rf_substituteList(SEXP el, SEXP rho)
{
    SEXP h, p = nullptr, res = nullptr;

    if (Rf_isNull(el)) return el;

    while (el != nullptr) {
	/* walk along the pairlist, substituting elements.
	   res is the result
	   p is the current last element
	   h is the element currently being processed
	 */
	if (CAR(el) == Symbols::DotsSymbol) {
	    if (rho == nullptr)
		h = R_UnboundValue;	/* so there is no substitution below */
	    else
		h = Rf_findVarInFrame3(rho, CAR(el), TRUE);
	    if (h == R_UnboundValue)
		h = PairList::cons(Symbols::DotsSymbol, nullptr);
	    else if (h == nullptr  || h == R_MissingArg)
		h = nullptr;
	    else if (TYPEOF(h) == DOTSXP)
		h = Rf_substituteList(h, nullptr);
	    else
 		Rf_error(_("'...' used in an incorrect context"));
	} else {
	    h = Rf_substitute(CAR(el), rho);
	    if (Rf_isLanguage(el))
		h = new Expression(h, {});
	    else
		h = PairList::cons(h, nullptr);
	    SET_TAG(h, TAG(el));
	}
	if (h != nullptr) {
	    if (res == nullptr)
		PROTECT(res = h);
	    else
		SETCDR(p, h);
	    /* now set 'p': dots might have expanded to a list of length > 1 */
	    while (CDR(h) != nullptr) h = CDR(h);
	    p = h;
	}
	el = CDR(el);
    }
    if(res != nullptr) UNPROTECT(1);
    return res;
}


/* This is a primitive SPECIALSXP */
HIDDEN SEXP do_substitute(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP expr, env;
    static GCRoot<ArgMatcher> matcher = new ArgMatcher({ "expr", "env" });

    /* argument matching */
    ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::RAW);
    matcher->match(arglist, { &expr, &env });

    /* set up the environment for substitution */
    if (env == R_MissingArg)
	env = rho;
    else
	env = Rf_eval(env, rho);
    if (env == R_GlobalEnv)	/* For historical reasons, don't substitute in R_GlobalEnv */
	env = nullptr;
    else if (TYPEOF(env) == VECSXP)
	env = Rf_NewEnvironment(nullptr, Rf_VectorToPairList(env), R_BaseEnv);
    else if (TYPEOF(env) == LISTSXP)
	env = Rf_NewEnvironment(nullptr, Rf_duplicate(env), R_BaseEnv);
    if (env != nullptr && TYPEOF(env) != ENVSXP)
	Rf_errorcall(call, _("invalid environment specified"));

    SEXP t = PairList::cons(Rf_duplicate(expr), nullptr);
    SEXP s = Rf_substituteList(t, env);
    return CAR(s);
}

/* This is a primitive SPECIALSXP */
HIDDEN SEXP do_quote(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP val = CAR(args);
    /* Make sure expression has NAMED == NAMEDMAX before being returning
       in order to avoid modification of source code */
    ENSURE_NAMEDMAX(val);
    return(val);
}

struct classType {
    const char *s;
    SEXPTYPE sexp;
    Rboolean canChange;
};

static classType classTable[] = {
    { "logical",	LGLSXP,	   TRUE },
    { "integer",	INTSXP,	   TRUE },
    { "double",		REALSXP,   TRUE },
    { "raw",		RAWSXP,    TRUE },
    { "complex",	CPLXSXP,   TRUE },
    { "character",	STRSXP,	   TRUE },
    { "expression",	EXPRSXP,   TRUE },
    { "list",		VECSXP,	   TRUE },
    { "environment",    ENVSXP,    FALSE },
    { "char",		CHARSXP,   TRUE },
    { "externalptr",	EXTPTRSXP,  FALSE },
    { "weakref",	WEAKREFSXP, FALSE },
    { "name",		SYMSXP,	   FALSE },

    { nullptr,	        SEXPTYPE(-1), FALSE}
};

static int class2type(const char *s)
{
    /* return the type if the class string is one of the basic types, else -1.
       Note that this is NOT Rf_str2type:  only certain types are defined to be basic
       classes; e.g., "language" is a type but many classes correspond to objects of
       this type.
    */
    int i; const char *si;
    for(i = 0; ; i++) {
	si = classTable[i].s;
	if(!si)
	    return -1;
	if(streql(s, si))
	    return i;
    }
    /* cannot get here return -1; */
}

static SEXP do_unsetS4(SEXP obj, SEXP newClass)
{
  if(Rf_isNull(newClass))  { /* NULL class is only valid for S3 objects */
    Rf_warning(_("Setting class(x) to NULL;   result will no longer be an S4 object"));
  }
  else if(Rf_length(newClass) > 1)
    Rf_warning(_("Setting class(x) to multiple strings (\"%s\", \"%s\", ...); result will no longer be an S4 object"),
	    Rf_translateChar(STRING_ELT(newClass, 0)),
	    Rf_translateChar(STRING_ELT(newClass, 1)));
  else
    Rf_warning(_("Setting class(x) to \"%s\" sets attribute to NULL; result will no longer be an S4 object"),
	    R_CHAR(Rf_asChar(newClass)));
  UNSET_S4_OBJECT(obj);
  return obj;
}

/* set the class to value, and return the modified object.  This is
   NOT a primitive assignment operator , because there is no code in R
   that changes type in place. */
static SEXP R_set_class(SEXP obj, SEXP value, SEXP call)
{
    int nProtect = 0;
    // use of zero-length vector used to be documented.
    if(!Rf_length(value)) { // usually NULL
	Rf_setAttrib(obj, Symbols::ClassSymbol, value);
	if(IS_S4_OBJECT(obj)) /* NULL class is only valid for S3 objects */
	  do_unsetS4(obj, value);
	return obj;
    }
    if(TYPEOF(value) != STRSXP) {
	SEXP dup;
	/* assumes value is protected, which it is in R_do_set_class */
	PROTECT(dup = Rf_duplicate(value));
	PROTECT(value = Rf_coerceVector(dup, STRSXP));
	nProtect += 2;
    }
    if(Rf_length(value) > 1) {
	Rf_setAttrib(obj, Symbols::ClassSymbol, value);
	if(IS_S4_OBJECT(obj)) /*  multiple strings only valid for S3 objects */
	  do_unsetS4(obj, value);
    }
    else if(Rf_length(value) == 0) {
	UNPROTECT(nProtect); nProtect = 0;
	Rf_error(_("invalid replacement object to be a class string"));
    }
    else {
	const char *valueString;
	int whichType;

	SEXP cur_class; SEXPTYPE valueType;
	valueString = R_CHAR(Rf_asChar(value)); /* ASCII */
	whichType = class2type(valueString);
	valueType = (whichType == -1) ? (SEXPTYPE) -1 : classTable[whichType].sexp;
	PROTECT(cur_class = R_data_class(obj, FALSE)); nProtect++;
	/*  assigning type as a class deletes an explicit class attribute. */
	if(valueType != (SEXPTYPE)-1) {
            SEXPTYPE valueType = classTable[whichType].sexp;
	    Rf_setAttrib(obj, Symbols::ClassSymbol, nullptr);
	    if(IS_S4_OBJECT(obj)) /* NULL class is only valid for S3 objects */
	      do_unsetS4(obj, value);
	    if(classTable[whichType].canChange) {
		PROTECT(obj = ascommon(call, obj, valueType));
		nProtect++;
	    }
	    else if(valueType != TYPEOF(obj))
		Rf_error(_("\"%s\" can only be set as the class if the object has this type; found \"%s\""),
		      valueString, Rf_type2char(TYPEOF(obj)));
	    /* else, leave alone */
	}
	else if(streql("numeric", valueString)) {
	    Rf_setAttrib(obj, Symbols::ClassSymbol, nullptr);
	    if(IS_S4_OBJECT(obj)) /* NULL class is only valid for S3 objects */
	      do_unsetS4(obj, value);
	    switch(TYPEOF(obj)) {
	    case INTSXP: case REALSXP: break;
	    default: PROTECT(obj = Rf_coerceVector(obj, REALSXP));
		nProtect++;
	    }
	}
	/* the next 2 special cases mirror the special code in
	 * R_data_class */
	else if(streql("matrix", valueString)) {
	    if(Rf_length(Rf_getAttrib(obj, Symbols::DimSymbol)) != 2)
		Rf_error(_("invalid to set the class to matrix unless the dimension attribute is of length 2 (was %d)"),
		 Rf_length(Rf_getAttrib(obj, Symbols::DimSymbol)));
	    Rf_setAttrib(obj, Symbols::ClassSymbol, nullptr);
	    if(IS_S4_OBJECT(obj))
	      do_unsetS4(obj, value);
	}
	else if(streql("array", valueString)) {
	    if(Rf_length(Rf_getAttrib(obj, Symbols::DimSymbol)) <= 0)
		Rf_error(_("cannot set class to \"array\" unless the dimension attribute has length > 0"));
	    Rf_setAttrib(obj, Symbols::ClassSymbol, nullptr);
	    if(IS_S4_OBJECT(obj)) /* NULL class is only valid for S3 objects */
	      UNSET_S4_OBJECT(obj);
	}
	else { /* set the class but don't do the coercion; that's
		  supposed to be done by an as() method */
	    Rf_setAttrib(obj, Symbols::ClassSymbol, value);
	}
    }
    UNPROTECT(nProtect);
    return obj;
}

HIDDEN SEXP R_do_set_class(/*const*/ Expression* call, const BuiltInFunction* op, RObject* object, RObject* klass)
{
    return R_set_class(object, klass, call);
}

/* primitive */
HIDDEN SEXP do_storage_mode(/*const*/ Expression* call, const BuiltInFunction* op, RObject* obj, RObject* value)
{
/* storage.mode(obj) <- value */
    SEXP ans;
    SEXPTYPE type;

    if (!Rf_isValidString(value) || STRING_ELT(value, 0) == R_NaString)
	Rf_error(_("'value' must be non-null character string"));
    type = Rf_str2type(R_CHAR(STRING_ELT(value, 0)));
    if(type == SEXPTYPE( -1)) {
	/* For backwards compatibility we allow "real" and "single" */
	if(streql(R_CHAR(STRING_ELT(value, 0)), "real")) {
	    Rf_error("use of 'real' is defunct: use 'double' instead");
	} else if(streql(R_CHAR(STRING_ELT(value, 0)), "single")) {
	    Rf_error("use of 'single' is defunct: use mode<- instead");
	} else
	    Rf_error(_("invalid value"));
    }
    if(TYPEOF(obj) == type) return obj;
    if(Rf_isFactor(obj))
	Rf_error(_("invalid to change the storage mode of a factor"));
    PROTECT(ans = Rf_coerceVector(obj, type));
    SHALLOW_DUPLICATE_ATTRIB(ans, obj);
    UNPROTECT(1);
    return ans;
}

#include "rho/ArgList.hpp"

const Symbol* ArgList::coerceTag(const RObject* tag)
{
	assert(tag);
    const char* symname = nullptr;
    if (tag->sexptype() == STRSXP) {
	const StringVector* strv = static_cast<const StringVector*>(tag);
	if (strv->size() >= 1) {
	    const String* strv0 = (*strv)[0];
	    if (strv0 && strv0->size() >= 1)
		symname = Rf_translateChar(const_cast<String*>(strv0));
	}
    }
    if (!symname) {
	StringVector* strv
	    = static_cast<StringVector*>(Rf_deparse1(const_cast<RObject*>(tag),
						     TRUE, SIMPLEDEPARSE));
	symname = (*strv)[0]->c_str();
    }
    return Symbol::obtain(symname);
}
