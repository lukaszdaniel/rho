/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2017   The R Core Team
 *  Copyright (C) 2002-2015   The R Foundation
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Localization.h>
#include <Internal.h>
#include <Rmath.h>
#include <R_ext/RS.h>     /* for Calloc/Free */
#include <R_ext/Applic.h> /* for dgemm */
#include <R_ext/Itermacros.h>

#include "duplicate.h"

#include <complex>
#include "Rcomplex.h"	/* toC99 */

#include "rho/BuiltInFunction.hpp"
#include "rho/GCStackRoot.hpp"
#include "rho/RAllocStack.hpp"
#include "rho/Subscripting.hpp"
#include "rho/BuiltInFunction.hpp"

using namespace rho;

/* "GetRowNames" and "GetColNames" are utility routines which
 * locate and return the row names and column names from the
 * dimnames attribute of a matrix.  They are useful because
 * old versions of R used pair-based lists for dimnames
 * whereas recent versions use vector based lists.

 * These are now very old, plus
 * ``When the "dimnames" attribute is
 *   grabbed off an array it is always adjusted to be a vector.''

 They are used in bind.cpp and subset.cpp, and advertised in Rinternals.h
*/
SEXP Rf_GetRowNames(SEXP dimnames)
{
    if (TYPEOF(dimnames) == VECSXP)
	return VECTOR_ELT(dimnames, 0);
    else
	return R_NilValue;
}

SEXP Rf_GetColNames(SEXP dimnames)
{
    if (TYPEOF(dimnames) == VECSXP)
	return VECTOR_ELT(dimnames, 1);
    else
	return R_NilValue;
}

SEXP attribute_hidden do_matrix(/*const*/ Expression* call, const BuiltInFunction* op, RObject* vals, RObject* nrow, RObject* ncol, RObject* byrow_, RObject* dimnames, RObject* miss_nr_, RObject* miss_nc_)
{
    SEXP ans;
    int nr = 1, nc = 1;

    switch(TYPEOF(vals)) {
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	case RAWSXP:
	case EXPRSXP:
	case VECSXP:
	    break;
	default:
	    Rf_error(_("'data' must be of a vector type, was '%s'"),
		Rf_type2char(TYPEOF(vals)));
    }
    R_xlen_t lendat = XLENGTH(vals);
    int byrow = Rf_asLogical(byrow_);
    if (byrow == NA_INTEGER)
	Rf_error(_("invalid '%s' argument"), "byrow");
    int miss_nr = Rf_asLogical(miss_nr_);
    int miss_nc = Rf_asLogical(miss_nc_);

    if (!miss_nr) {
	if (!Rf_isNumeric(nrow)) Rf_error(_("non-numeric matrix extent"));
	nr = Rf_asInteger(nrow);
	if (nr == NA_INTEGER)
	    Rf_error(_("invalid 'nrow' value (too large or NA)"));
	if (nr < 0)
	    Rf_error(_("invalid 'nrow' value (< 0)"));
    }
    if (!miss_nc) {
	if (!Rf_isNumeric(ncol)) Rf_error(_("non-numeric matrix extent"));
	nc = Rf_asInteger(ncol);
	if (nc == NA_INTEGER)
	    Rf_error(_("invalid 'ncol' value (too large or NA)"));
	if (nc < 0)
	    Rf_error(_("invalid 'ncol' value (< 0)"));
    }
    if (miss_nr && miss_nc) {
	if (lendat > INT_MAX) Rf_error("data is too long");
	nr = int(lendat);
    } else if (miss_nr) {
	if (lendat > (double) nc * INT_MAX) Rf_error("data is too long");
	// avoid division by zero
	if (nc == 0) {
	    if (lendat) Rf_error(_("nc = 0 for non-null data"));
	    else nr = 0;
	} else
	    nr = (int) ceil((double) lendat / (double) nc);
    } else if (miss_nc) {
	if (lendat > (double) nr * INT_MAX) Rf_error("data is too long");
	// avoid division by zero
	if (nr == 0) {
	    if (lendat) Rf_error(_("nr = 0 for non-null data"));
	    else nc = 0;
	} else
	    nc = (int) ceil((double) lendat / (double) nr);
    }

    if(lendat > 0) {
	R_xlen_t nrc = R_xlen_t(nr) * nc;
	if (lendat > 1 && nrc % lendat != 0) {
	    if (((lendat > nr) && (lendat / nr) * nr != lendat) ||
		((lendat < nr) && (nr / lendat) * lendat != nr))
		Rf_warning(_("data length [%d] is not a sub-multiple or multiple of the number of rows [%d]"), lendat, nr);
	    else if (((lendat > nc) && (lendat / nc) * nc != lendat) ||
		     ((lendat < nc) && (nc / lendat) * lendat != nc))
		Rf_warning(_("data length [%d] is not a sub-multiple or multiple of the number of columns [%d]"), lendat, nc);
	}
	else if ((lendat > 1) && (nrc == 0)){
	    Rf_warning(_("data length exceeds size of matrix"));
	}
    }

#ifndef LONG_VECTOR_SUPPORT
    if (double(nr) * double(nc) > INT_MAX)
	Rf_error(_("too many elements specified"));
#endif

    PROTECT(ans = Rf_allocMatrix(TYPEOF(vals), nr, nc));
    if(lendat) {
	if (Rf_isVector(vals))
	    Rf_copyMatrix(ans, vals, Rboolean(byrow));
	else
	    Rf_copyListMatrix(ans, vals, Rboolean(byrow));
    } else if (Rf_isVector(vals)) { /* fill with NAs */
	R_xlen_t N = R_xlen_t(nr) * nc, i;
	switch(TYPEOF(vals)) {
	case STRSXP:
	    for (i = 0; i < N; i++)
		SET_STRING_ELT(ans, i, NA_STRING);
	    break;
	case LGLSXP:
	    for (i = 0; i < N; i++)
		LOGICAL(ans)[i] = NA_LOGICAL;
	    break;
	case INTSXP:
	    for (i = 0; i < N; i++)
		INTEGER(ans)[i] = NA_INTEGER;
	    break;
	case REALSXP:
	    for (i = 0; i < N; i++)
		REAL(ans)[i] = NA_REAL;
	    break;
	case CPLXSXP:
	    {
		Rcomplex na_cmplx;
		na_cmplx.r = NA_REAL;
		na_cmplx.i = 0;
		for (i = 0; i < N; i++)
		    COMPLEX(ans)[i] = na_cmplx;
	    }
	    break;
	case RAWSXP:
	    if (N) memset(RAW(ans), 0, N);
	    break;
	default:
	    /* don't fill with anything */
	    ;
	}
    }
    if(!Rf_isNull(dimnames) && Rf_length(dimnames) > 0)
	ans = Rf_dimnamesgets(ans, dimnames);
    UNPROTECT(1);
    return ans;
}


SEXP Rf_allocMatrix(SEXPTYPE mode, int nrow, int ncol)
{
    SEXP s, t;
    R_xlen_t n;

    if (nrow < 0 || ncol < 0)
	Rf_error(_("negative extents to matrix"));
#ifndef LONG_VECTOR_SUPPORT
    if (double(nrow) * double(ncol) > INT_MAX)
	Rf_error(_("allocMatrix: too many elements specified"));
#endif
    n = (R_xlen_t(nrow)) * ncol;
    PROTECT(s = Rf_allocVector(mode, n));
    PROTECT(t = Rf_allocVector(INTSXP, 2));
    INTEGER(t)[0] = nrow;
    INTEGER(t)[1] = ncol;
    Rf_setAttrib(s, R_DimSymbol, t);
    UNPROTECT(2);
    return s;
}

/**
 * @brief Allocate a 3-dimensional array
 *
 * @param mode The R mode (e.g. INTSXP)
 * @param nrow number of rows
 * @param ncol number of columns
 * @param nface number of faces
 *
 * @return A 3-dimensional array of the indicated dimensions and mode
 */
SEXP Rf_alloc3DArray(SEXPTYPE mode, int nrow, int ncol, int nface)
{
    SEXP s, t;
    R_xlen_t n;

    if (nrow < 0 || ncol < 0 || nface < 0)
	Rf_error(_("negative extents to 3D array"));
#ifndef LONG_VECTOR_SUPPORT
    if (double(nrow) * double(ncol) * double(nface) > INT_MAX)
	Rf_error(_("alloc3Darray: too many elements specified"));
#endif
    n = (R_xlen_t(nrow)) * ncol * nface;
    PROTECT(s = Rf_allocVector(mode, n));
    PROTECT(t = Rf_allocVector(INTSXP, 3));
    INTEGER(t)[0] = nrow;
    INTEGER(t)[1] = ncol;
    INTEGER(t)[2] = nface;
    Rf_setAttrib(s, R_DimSymbol, t);
    UNPROTECT(2);
    return s;
}


SEXP Rf_allocArray(SEXPTYPE mode, SEXP dims)
{
    SEXP array;
    int i;
    R_xlen_t n = 1;
    double dn = 1;

    for (i = 0; i < LENGTH(dims); i++) {
	dn *= INTEGER(dims)[i];
#ifndef LONG_VECTOR_SUPPORT
	if(dn > INT_MAX)
	    Rf_error(_("'allocArray': too many elements specified by 'dims'"));
#endif
	n *= INTEGER(dims)[i];
    }

    PROTECT(dims = Rf_duplicate(dims));
    PROTECT(array = Rf_allocVector(mode, n));
    Rf_setAttrib(array, R_DimSymbol, dims);
    UNPROTECT(2);
    return array;
}

/* DropDims strips away redundant dimensioning information. */
/* If there is an appropriate dimnames attribute the correct */
/* element is extracted and attached to the vector as a names */
/* attribute.  Note that this function mutates x. */
/* Duplication should occur before this is called. */

SEXP Rf_DropDims(SEXP x)
{
    // The following is a kludge.  It is possible that the object
    // pointed to by x is not actually a VectorBase; however, provided
    // its 'dims' and/or 'dimnames' attributes, if present, are
    // configured in the standard way, Subscripting::dropDimensions()
    // should behave correctly.
    VectorBase* vb = static_cast<VectorBase*>(x);
    Subscripting::dropDimensions(vb);
    return vb;
}

SEXP attribute_hidden do_drop(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_)
{
    GCStackRoot<> x;
    SEXP xdims;
    int i, n, shorten;

    x = x_;
    if ((xdims = Rf_getAttrib(x, R_DimSymbol)) != R_NilValue) {
	n = LENGTH(xdims);
	shorten = 0;
	for (i = 0; i < n; i++)
	    if (INTEGER(xdims)[i] == 1) shorten = 1;
	if (shorten) {
	    if (MAYBE_REFERENCED(x)) x = Rf_duplicate(x);
	    x = Rf_DropDims(x);
	}
    }
    return x;
}

/* Length of Primitive Objects */

SEXP attribute_hidden do_length(/*const*/ Expression* call, const BuiltInFunction* op, Environment* rho, RObject* const* args, int num_args, const PairList* tags)
{
    SEXP x = args[0];
    assert(num_args == 1);

    auto dispatched = op->InternalDispatch(
        call, rho, ArgList({ x }, ArgList::EVALUATED));
    if (dispatched.first) {
	RObject* ans = dispatched.second;
	if (Rf_length(ans) == 1 && TYPEOF(ans) == REALSXP) {
	    GCStackRoot<> ansrt(ans);
	    double d = REAL(ans)[0];
	    if (R_FINITE(d) && d >= 0. && d <= INT_MAX && floor(d) == d) {
                PROTECT(ans);
                ans = Rf_coerceVector(ans, INTSXP);
                UNPROTECT(1);
                return(ans);
            }
	}
	return(ans);
    }


#ifdef LONG_VECTOR_SUPPORT
    // or use IS_LONG_VEC
    R_xlen_t len = Rf_xlength(x);
    if (len > INT_MAX) return Rf_ScalarReal(double(len));
#endif
    return Rf_ScalarInteger(Rf_length(x));
}

R_xlen_t attribute_hidden dispatch_xlength(RObject* x,
                                           const Expression* /*call*/,
                                           Environment* rho)
{
    static BuiltInFunction* length_op
	= BuiltInFunction::obtainPrimitive("length");
    static GCRoot<Expression> length_call = SEXP_downcast<Expression*>(
        Rf_lang2(Rf_install("length"), Rf_install("x")));

    if (Rf_isObject(x))
    {
	auto dispatched = length_op->InternalDispatch(
	    length_call, rho, ArgList({ x }, ArgList::EVALUATED));
	if (dispatched.first) {
	    RObject* len = dispatched.second;
	    return (R_xlen_t)
		(TYPEOF(len) == REALSXP ? REAL(len)[0] : Rf_asInteger(len));
	}
    }
    return(Rf_xlength(x));
}

// auxiliary for do_lengths_*(), i.e., R's lengths()
static R_xlen_t getElementLength(RObject* x, R_xlen_t i, Expression* call,
				 Environment* rho) {
    SEXP x_elt = dispatch_subset2(x, i, call, rho);
    return(dispatch_xlength(x_elt, call, rho));
}

#ifdef LONG_VECTOR_SUPPORT
static SEXP do_lengths_long(SEXP x, Expression* call, Environment* rho)
{
    SEXP ans;
    R_xlen_t x_len, i;
    double *ans_elt;

    x_len = dispatch_xlength(x, call, rho);
    PROTECT(ans = Rf_allocVector(REALSXP, x_len));
    for (i = 0, ans_elt = REAL(ans); i < x_len; i++, ans_elt++) {
        *ans_elt = double(getElementLength(x, i, call, rho));
    }
    UNPROTECT(1);
    return ans;
}
#endif

SEXP attribute_hidden do_lengths(/*const*/ Expression* call, const BuiltInFunction* op, Environment* rho, RObject* const* args, int num_args, const PairList* tags)
{
    SEXP x = args[0], ans;
    R_xlen_t x_len, i;
    int *ans_elt;
    int useNames = Rf_asLogical(args[1]);
    if (useNames == NA_LOGICAL)
	Rf_error(_("invalid '%s' value"), "use.names");
    bool isList_ = Rf_isVectorList(x) || Rf_isS4(x);
    if(!isList_) switch(TYPEOF(x)) {
	case NILSXP:
	case CHARSXP:
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	case RAWSXP:
	    break;
	default:
	    Rf_error(_("'%s' must be a list or atomic vector"), "x");
    }
    x_len = dispatch_xlength(x, call, rho);
    PROTECT(ans = Rf_allocVector(INTSXP, x_len));
    if(isList_) {
	for (i = 0, ans_elt = INTEGER(ans); i < x_len; i++, ans_elt++) {
	    R_xlen_t x_elt_len = getElementLength(x, i, call, rho);
#ifdef LONG_VECTOR_SUPPORT
	    if (x_elt_len > INT_MAX) {
		ans = do_lengths_long(x, call, rho);
		UNPROTECT(1);
		PROTECT(ans);
		break;
	    }
#endif
	    *ans_elt = (int)x_elt_len;
	}
    } else { // atomic: every element has length 1
	for (i = 0, ans_elt = INTEGER(ans); i < x_len; i++, ans_elt++)
	    *ans_elt = 1;
    }
    SEXP dim = Rf_getAttrib(x, R_DimSymbol);
    if(!Rf_isNull(dim)) {
        Rf_setAttrib(ans, R_DimSymbol, dim);
    }
    if(useNames) {
	SEXP names = Rf_getAttrib(x, R_NamesSymbol);
	if(!Rf_isNull(names)) Rf_setAttrib(ans, R_NamesSymbol, names);
    SEXP dimnames = Rf_getAttrib(x, R_DimNamesSymbol);
    if(!Rf_isNull(dimnames)) Rf_setAttrib(ans, R_DimNamesSymbol, dimnames);	
    }
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_rowscols(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_)
{
    SEXP x, ans;
    int i, j, nr, nc;

    /* This is the dimensions vector */
    x = x_;
    if (!Rf_isInteger(x) || LENGTH(x) != 2)
	Rf_error(_("a matrix-like object is required as argument to '%s'"),
	      (op->variant() == 2) ? "col" : "row");

    nr = INTEGER(x)[0];
    nc = INTEGER(x)[1];

    ans = Rf_allocMatrix(INTSXP, nr, nc);

    R_xlen_t NR = nr;
    switch (op->variant()) {
    case 1:
	for (i = 0; i < nr; i++)
	    for (j = 0; j < nc; j++)
		INTEGER(ans)[i + j * NR] = i + 1;
	break;
    case 2:
	for (i = 0; i < nr; i++)
	    for (j = 0; j < nc; j++)
		INTEGER(ans)[i + j * NR] = j + 1;
	break;
    }
    return ans;
}

/*
 Whenever vector x contains NaN or Inf (or -Inf), the function returns TRUE.
 It can be imprecise: it can return TRUE in other cases as well.

 A precise version of the function could be implemented as

       for (R_xlen_t i = 0; i < n; i++)
           if (!R_FINITE(x[i])) return TRUE;
       return FALSE;

 The present version is imprecise, but faster.
*/
static Rboolean mayHaveNaNOrInf(double *x, R_xlen_t n)
{
    if ((n&1) != 0 && !R_FINITE(x[0]))
	return TRUE;
    for (R_xlen_t i = n&1; i < n; i += 2)
	/* A precise version could use this condition:
	 *
	 * !R_FINITE(x[i]+x[i+1]) && (!R_FINITE(x[i]) || !R_FINITE(x[i+1]))
	 *
	 * The present imprecise version has been found to be faster
	 * with GCC and ICC in the common case when the sum of the two
	 * values is always finite.
	 *
	 * The present version is imprecise because the sum of two very
	 * large finite values (e.g. 1e308) may be infinite.
	 */
	if (!R_FINITE(x[i]+x[i+1]))
	    return TRUE;
    return FALSE;
}

/*
 This is an experimental version that has been observed to run fast on some
 SIMD hardware with GCC and ICC.
 Note that the OpenMP reduction assumes associativity of addition, which is
 safe here, because the result is only used for an imprecise test for
 the presence of NaN and Inf values.
*/
static Rboolean mayHaveNaNOrInf_simd(double *x, R_xlen_t n)
{
    double s = 0;
    /* SIMD reduction is supported since OpenMP 4.0. The value of _OPENMP is
       unreliable in some compilers, so we depend on HAVE_OPENMP_SIMDRED,
       which is normally set by configure based on a test. */
    /* _OPENMP >= 201307 */
#if defined(_OPENMP) && HAVE_OPENMP_SIMDRED
    #pragma omp simd reduction(+:s)
#endif
    for (R_xlen_t i = 0; i < n; i++)
	s += x[i];
    return Rboolean(!R_FINITE(s));
}

static Rboolean cmayHaveNaNOrInf(Rcomplex *x, R_xlen_t n)
{
    /* With HAVE_FORTRAN_DOUBLE_COMPLEX set, it should be clear that
       Rcomplex has no padding, so we could probably use mayHaveNaNOrInf,
       but better safe than sorry... */
    if ((n&1) != 0 && (!R_FINITE(x[0].r) || !R_FINITE(x[0].i)))
	return TRUE;
    for (R_xlen_t i = n&1; i < n; i += 2)
	if (!R_FINITE(x[i].r+x[i].i+x[i+1].r+x[i+1].i))
	    return TRUE;
    return FALSE;
}

/* experimental version for SIMD hardware (see also mayHaveNaNOrInf_simd) */
static Rboolean cmayHaveNaNOrInf_simd(Rcomplex *x, R_xlen_t n)
{
    double s = 0;
    /* _OPENMP >= 201307 - see mayHaveNaNOrInf_simd */
#if defined(_OPENMP) && HAVE_OPENMP_SIMDRED
    #pragma omp simd reduction(+:s)
#endif
    for (R_xlen_t i = 0; i < n; i++) {
	s += x[i].r;
	s += x[i].i;
    }
    return Rboolean(!R_FINITE(s));
}

static void internal_matprod(double *x, int nrx, int ncx,
                             double *y, int nry, int ncy, double *z)
{
    LDOUBLE sum;
#define MATPROD_BODY					\
    R_xlen_t NRX = nrx, NRY = nry;			\
    for (int i = 0; i < nrx; i++)			\
	for (int k = 0; k < ncy; k++) {			\
	    sum = 0.0;					\
	    for (int j = 0; j < ncx; j++)		\
		sum += x[i + j * NRX] * y[j + k * NRY];	\
	    z[i + k * NRX] = (double) sum;		\
	}
    MATPROD_BODY;
}

static void simple_matprod(double *x, int nrx, int ncx,
                           double *y, int nry, int ncy, double *z)
{
    double sum;
    MATPROD_BODY;
}

static void internal_crossprod(double *x, int nrx, int ncx,
                               double *y, int nry, int ncy, double *z)
{
    LDOUBLE sum;
#define CROSSPROD_BODY					\
    R_xlen_t NRX = nrx, NRY = nry, NCX = ncx;		\
    for (int i = 0; i < ncx; i++)			\
	for (int k = 0; k < ncy; k++) {			\
	    sum = 0.0;					\
	    for (int j = 0; j < nrx; j++)		\
		sum += x[j + i * NRX] * y[j + k * NRY];	\
	    z[i + k * NCX] = (double) sum;		\
	}
    CROSSPROD_BODY;
}

static void simple_crossprod(double *x, int nrx, int ncx,
                             double *y, int nry, int ncy, double *z)
{
    double sum;
    CROSSPROD_BODY;
}

static void internal_tcrossprod(double *x, int nrx, int ncx,
                                double *y, int nry, int ncy, double *z)
{
    LDOUBLE sum;
#define TCROSSPROD_BODY					\
    R_xlen_t NRX = nrx, NRY = nry;			\
    for (int i = 0; i < nrx; i++)			\
	for (int k = 0; k < nry; k++) {			\
	    sum = 0.0;					\
	    for (int j = 0; j < ncx; j++)		\
		sum += x[i + j * NRX] * y[k + j * NRY];	\
	    z[i + k * NRX] = (double) sum;		\
	}
    TCROSSPROD_BODY;
}

static void simple_tcrossprod(double *x, int nrx, int ncx,
                              double *y, int nry, int ncy, double *z)
{
    double sum;
    TCROSSPROD_BODY;
}

static void matprod(double *x, int nrx, int ncx,
		    double *y, int nry, int ncy, double *z)
{
    R_xlen_t NRX = nrx, NRY = nry;
    if (nrx == 0 || ncx == 0 || nry == 0 || ncy == 0) {
	/* zero-extent operations should return zeroes */
	for(R_xlen_t i = 0; i < NRX*ncy; i++) z[i] = 0;
	return;
    }

    switch(R_Matprod) {
	case MATPROD_DEFAULT:
	/* Don't trust the BLAS to handle NA/NaNs correctly: PR#4582
	 * The test is only O(n) here.
	 *
	 * MKL disclaimer: "LAPACK routines assume that input matrices
	 * do not contain IEEE 754 special values such as INF or NaN values.
	 * Using these special values may cause LAPACK to return unexpected
	 * results or become unstable."
	 */
	    if (mayHaveNaNOrInf(x, NRX*ncx) || mayHaveNaNOrInf(y, NRY*ncy)) {
		simple_matprod(x, nrx, ncx, y, nry, ncy, z);
		return;
	    }
	    break; /* use blas */
	case MATPROD_INTERNAL:
	    internal_matprod(x, nrx, ncx, y, nry, ncy, z);
	    return;
	case MATPROD_BLAS:
	    break;
	case MATPROD_DEFAULT_SIMD:
	    if (mayHaveNaNOrInf_simd(x, NRX*ncx) ||
		    mayHaveNaNOrInf_simd(y, NRY*ncy)) {
		simple_matprod(x, nrx, ncx, y, nry, ncy, z);
		return;
	    }
	    break; /* use blas */
    }

    const char *transT = "T", *transN = "N";
    double one = 1.0, zero = 0.0;
    int ione = 1;

    if (ncy == 1) /* matrix-vector or dot product */
	F77_CALL(dgemv)(transN, &nrx, &ncx, &one, x,
			&nrx, y, &ione, &zero, z, &ione);
    else if (nrx == 1) /* vector-matrix */
	/* Instead of xY, compute (xY)^T == (Y^T)(x^T)
	   The result is a vector, so transposing its content is no-op */
	F77_CALL(dgemv)(transT, &nry, &ncy, &one, y,
			&nry, x, &ione, &zero, z, &ione);
    else /* matrix-matrix or outer product */
	F77_CALL(dgemm)(transN, transN, &nrx, &ncy, &ncx, &one,
			x, &nrx, y, &nry, &zero, z, &nrx);
}

static void internal_cmatprod(Rcomplex *x, int nrx, int ncx,
                              Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
    LDOUBLE sum_i, sum_r;
#define CMATPROD_BODY					    \
    int i, j, k;					    \
    std::complex<double> xij, yjk;				    \
    R_xlen_t NRX = nrx, NRY = nry;			    \
    for (i = 0; i < nrx; i++)				    \
	for (k = 0; k < ncy; k++) {			    \
	    sum_r = 0.0;				    \
	    sum_i = 0.0;				    \
	    for (j = 0; j < ncx; j++) {			    \
		xij = toC99(x + (i + j * NRX));		    \
		yjk = toC99(y + (j + k * NRY));		    \
		sum_r += (xij * yjk).real();		    \
		sum_i += (xij * yjk).imag();		    \
	    }						    \
	    z[i + k * NRX].r = (double) sum_r;		    \
	    z[i + k * NRX].i = (double) sum_i;		    \
	}
    CMATPROD_BODY;
}

static void simple_cmatprod(Rcomplex *x, int nrx, int ncx,
                            Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
    double sum_i, sum_r;
    CMATPROD_BODY;
}

static void internal_ccrossprod(Rcomplex *x, int nrx, int ncx,
                                Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
    LDOUBLE sum_i, sum_r;
#define CCROSSPROD_BODY					    \
    int i, j, k;					    \
    std::complex<double> xji, yjk;				    \
    R_xlen_t NRX = nrx, NRY = nry, NCX = ncx;		    \
    for (i = 0; i < ncx; i++)				    \
	for (k = 0; k < ncy; k++) {			    \
	    sum_r = 0.0;				    \
	    sum_i = 0.0;				    \
	    for (j = 0; j < nrx; j++) {			    \
		xji = toC99(x + (j + i * NRX));		    \
		yjk = toC99(y + (j + k * NRY));		    \
		sum_r += (xji * yjk).real();		    \
		sum_i += (xji * yjk).imag();		    \
	    }						    \
	    z[i + k * NCX].r = (double) sum_r;		    \
	    z[i + k * NCX].i = (double) sum_i;		    \
	}
    CCROSSPROD_BODY;
}

static void simple_ccrossprod(Rcomplex *x, int nrx, int ncx,
                              Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
    double sum_i, sum_r;
    CCROSSPROD_BODY;
}

static void internal_tccrossprod(Rcomplex *x, int nrx, int ncx,
                                 Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
    LDOUBLE sum_i, sum_r;
#define TCCROSSPROD_BODY				    \
    int i, j, k;					    \
    std::complex<double> xij, ykj;				    \
    R_xlen_t NRX = nrx, NRY = nry;			    \
    for (i = 0; i < nrx; i++)				    \
	for (k = 0; k < nry; k++) {			    \
	    sum_r = 0.0;				    \
	    sum_i = 0.0;				    \
	    for (j = 0; j < ncx; j++) {			    \
		xij = toC99(x + (i + j * NRX));		    \
		ykj = toC99(y + (k + j * NRY));		    \
		sum_r += (xij * ykj).real();		    \
		sum_i += (xij * ykj).imag();		    \
	    }						    \
	    z[i + k * NRX].r = (double) sum_r;		    \
	    z[i + k * NRX].i = (double) sum_i;		    \
	}
    TCCROSSPROD_BODY;
}

static void simple_tccrossprod(Rcomplex *x, int nrx, int ncx,
                               Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
    double sum_i, sum_r;
    TCCROSSPROD_BODY;
}

static void cmatprod(Rcomplex *x, int nrx, int ncx,
		     Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
    R_xlen_t NRX = nrx, NRY = nry;
    if (nrx == 0 || ncx == 0 || nry == 0 || ncy == 0) {
	/* zero-extent operations should return zeroes */
	for(R_xlen_t i = 0; i < NRX*ncy; i++) z[i].r = z[i].i = 0;
	return;
    }

#ifndef HAVE_FORTRAN_DOUBLE_COMPLEX
    if (R_Matprod == MATPROD_INTERNAL)
	internal_cmatprod(x, nrx, ncx, y, nry, ncy, z);
    else
	simple_cmatprod(x, nrx, ncx, y, nry, ncy, z);
#else
    switch(R_Matprod) {
	case MATPROD_DEFAULT:
	    if (cmayHaveNaNOrInf(x, NRX*ncx) || cmayHaveNaNOrInf(y, NRY*ncy)) {
		simple_cmatprod(x, nrx, ncx, y, nry, ncy, z);
		return;
	    }
	    break; /* use blas */
	case MATPROD_INTERNAL:
	    internal_cmatprod(x, nrx, ncx, y, nry, ncy, z);
	    return;
	case MATPROD_BLAS:
	    break;
	case MATPROD_DEFAULT_SIMD:
	    if (cmayHaveNaNOrInf_simd(x, NRX*ncx) ||
		    cmayHaveNaNOrInf_simd(y, NRY*ncy)) {
		simple_cmatprod(x, nrx, ncx, y, nry, ncy, z);
		return;
	    }
	    break; /* use blas */
    }

    const char *transa = "N", *transb = "N";
    Rcomplex one, zero;
    one.r = 1.0; one.i = zero.r = zero.i = 0.0;

    F77_CALL(zgemm)(transa, transb, &nrx, &ncy, &ncx, &one,
                    x, &nrx, y, &nry, &zero, z, &nrx);
#endif
}

static void symcrossprod(double *x, int nr, int nc, double *z)
{
    R_xlen_t NR = nr, NC = nc;
    if (nr == 0 || nc == 0) {
	/* zero-extent operations should return zeroes */
	for(R_xlen_t i = 0; i < NC*NC; i++) z[i] = 0;
	return;
    }

    switch(R_Matprod) {
	case MATPROD_DEFAULT:
	    /* see matprod for more details */
	    if (mayHaveNaNOrInf(x, NR*nc)) {
		simple_crossprod(x, nr, nc, x, nr, nc, z);
		return;
	    }
	    break; /* use blas */
	case MATPROD_INTERNAL:
	    internal_crossprod(x, nr, nc, x, nr, nc, z);
	    return;
	case MATPROD_BLAS:
	    break;
	case MATPROD_DEFAULT_SIMD:
	    if (mayHaveNaNOrInf_simd(x, NR*nc))  {
		simple_crossprod(x, nr, nc, x, nr, nc, z);
		return;
	    }
	    break; /* use blas */
    }

    const char *trans = "T", *uplo = "U";
    double one = 1.0, zero = 0.0;

    F77_CALL(dsyrk)(uplo, trans, &nc, &nr, &one, x, &nr, &zero, z, &nc);
    for (int i = 1; i < nc; i++)
	for (int j = 0; j < i; j++) z[i + NC *j] = z[j + NC * i];
}

static void crossprod(double *x, int nrx, int ncx,
		      double *y, int nry, int ncy, double *z)
{
    R_xlen_t NRX = nrx, NRY = nry;
    if (nrx == 0 || ncx == 0 || nry == 0 || ncy == 0) {
	/* zero-extent operations should return zeroes */
	R_xlen_t NCX = ncx;
	for(R_xlen_t i = 0; i < NCX*ncy; i++) z[i] = 0;
	return;
    }

    switch(R_Matprod) {
	case MATPROD_DEFAULT:
	    /* see matprod for more details */
	    if (mayHaveNaNOrInf(x, NRX*ncx) || mayHaveNaNOrInf(y, NRY*ncy)) {
		simple_crossprod(x, nrx, ncx, y, nry, ncy, z);
		return;
	    }
	    break; /* use blas */
	case MATPROD_INTERNAL:
	    internal_crossprod(x, nrx, ncx, y, nry, ncy, z);
	    return;
	case MATPROD_BLAS:
	    break;
	case MATPROD_DEFAULT_SIMD:
	    if (mayHaveNaNOrInf_simd(x, NRX*ncx) ||
		    mayHaveNaNOrInf_simd(y, NRY*ncy)) {
		simple_crossprod(x, nrx, ncx, y, nry, ncy, z);
		return;
	    }
	    break; /* use blas */
    }

    const char *transT = "T", *transN = "N";
    double one = 1.0, zero = 0.0;
    int ione = 1;

    if (ncy == 1) /* matrix-vector or dot product */
	F77_CALL(dgemv)(transT, &nrx, &ncx, &one, x,
			&nrx, y, &ione, &zero, z, &ione);
    else if (ncx == 1) /* vector-matrix */
	/* Instead of (x^T)Y, compute ((x^T)Y)^T == (Y^T)x
	   The result is a vector, so transposing its content is no-op */
	F77_CALL(dgemv)(transT, &nry, &ncy, &one, y,
			&nry, x, &ione, &zero, z, &ione);
    else /* matrix-matrix  or outer product */
	F77_CALL(dgemm)(transT, transN, &ncx, &ncy, &nrx, &one,
		        x, &nrx, y, &nry, &zero, z, &ncx);
}

static void ccrossprod(Rcomplex *x, int nrx, int ncx,
		       Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
    R_xlen_t NRX = nrx, NRY = nry;
    if (nrx == 0 || ncx == 0 || nry == 0 || ncy == 0) {
	/* zero-extent operations should return zeroes */
	R_xlen_t NCX = ncx;
	for(R_xlen_t i = 0; i < NCX*ncy; i++) z[i].r = z[i].i = 0;
	return;
    }

#ifndef HAVE_FORTRAN_DOUBLE_COMPLEX
    if (R_Matprod == MATPROD_INTERNAL)
	internal_ccrossprod(x, nrx, ncx, y, nry, ncy, z);
    else
	simple_ccrossprod(x, nrx, ncx, y, nry, ncy, z);
#else
    switch(R_Matprod) {
	case MATPROD_DEFAULT:
	    if (cmayHaveNaNOrInf(x, NRX*ncx) || cmayHaveNaNOrInf(y, NRY*ncy)) {
		simple_ccrossprod(x, nrx, ncx, y, nry, ncy, z);
		return;
	    }
	    break; /* use blas */
	case MATPROD_INTERNAL:
	    internal_ccrossprod(x, nrx, ncx, y, nry, ncy, z);
	    return;
	case MATPROD_BLAS:
	    break;
	case MATPROD_DEFAULT_SIMD:
	    if (cmayHaveNaNOrInf_simd(x, NRX*ncx) ||
		    cmayHaveNaNOrInf_simd(y, NRY*ncy)) {
		simple_ccrossprod(x, nrx, ncx, y, nry, ncy, z);
		return;
	    }
	    break; /* use blas */
    }


    const char *transa = "T", *transb = "N";
    Rcomplex one, zero;
    one.r = 1.0; one.i = zero.r = zero.i = 0.0;

    F77_CALL(zgemm)(transa, transb, &ncx, &ncy, &nrx, &one,
                    x, &nrx, y, &nry, &zero, z, &ncx);
#endif
}

static void symtcrossprod(double *x, int nr, int nc, double *z)
{
    R_xlen_t NR = nr;
    if (nr == 0 || nc == 0) {
	/* zero-extent operations should return zeroes */
	for(R_xlen_t i = 0; i < NR*NR; i++) z[i] = 0;
	return;
    }

    switch(R_Matprod) {
	case MATPROD_DEFAULT:
	    /* see matprod for more details */
	    if (mayHaveNaNOrInf(x, NR*nc)) {
		simple_tcrossprod(x, nr, nc, x, nr, nc, z);
		return;
	    }
	    break; /* use blas */
	case MATPROD_INTERNAL:
	    internal_tcrossprod(x, nr, nc, x, nr, nc, z);
	    return;
	case MATPROD_BLAS:
	    break;
	case MATPROD_DEFAULT_SIMD:
	    if (mayHaveNaNOrInf_simd(x, NR*nc))  {
		simple_tcrossprod(x, nr, nc, x, nr, nc, z);
		return;
	    }
	    break; /* use blas */
    }

    const char *trans = "N", *uplo = "U";
    double one = 1.0, zero = 0.0;

    F77_CALL(dsyrk)(uplo, trans, &nr, &nc, &one, x, &nr, &zero, z, &nr);
    for (int i = 1; i < nr; i++)
	for (int j = 0; j < i; j++) z[i + nr *j] = z[j + nr * i];
}

static void tcrossprod(double *x, int nrx, int ncx,
		      double *y, int nry, int ncy, double *z)
{
    R_xlen_t NRX = nrx, NRY = nry;
    if (nrx == 0 || ncx == 0 || nry == 0 || ncy == 0) {
	/* zero-extent operations should return zeroes */
	for(R_xlen_t i = 0; i < NRX*nry; i++) z[i] = 0;
	return;
    }

    switch(R_Matprod) {
	case MATPROD_DEFAULT:
	    if (mayHaveNaNOrInf(x, NRX*ncx) || mayHaveNaNOrInf(y, NRY*ncy)) {
		simple_tcrossprod(x, nrx, ncx, y, nry, ncy, z);
		return;
	    }
	    break; /* use blas */
	case MATPROD_INTERNAL:
	    internal_tcrossprod(x, nrx, ncx, y, nry, ncy, z);
	    return;
	case MATPROD_BLAS:
	    break;
	case MATPROD_DEFAULT_SIMD:
	    if (mayHaveNaNOrInf_simd(x, NRX*ncx) ||
		    mayHaveNaNOrInf_simd(y, NRY*ncy)) {
		simple_tcrossprod(x, nrx, ncx, y, nry, ncy, z);
		return;
	    }
	    break; /* use blas */
    }

    const char *transT = "T", *transN = "N";
    double one = 1.0, zero = 0.0;
    int ione = 1;

    if (nry == 1) /* matrix-vector or dot product */
	F77_CALL(dgemv)(transN, &nrx, &ncx, &one, x,
			&nrx, y, &ione, &zero, z, &ione);
    else if (nrx == 1) /* vector-matrix */
	/* Instead of x(Y^T), compute (x(Y^T))^T == Y(x^T)
	   The result is a vector, so transposing its content is no-op */
	F77_CALL(dgemv)(transN, &nry, &ncy, &one, y,
			&nry, x, &ione, &zero, z, &ione);
    else /* matrix-matrix or outer product */
	F77_CALL(dgemm)(transN, transT, &nrx, &nry, &ncx, &one,
		    x, &nrx, y, &nry, &zero, z, &nrx);
}

static void tccrossprod(Rcomplex *x, int nrx, int ncx,
			Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
    R_xlen_t NRX = nrx, NRY = nry;
    if (nrx == 0 || ncx == 0 || nry == 0 || ncy == 0) {
	/* zero-extent operations should return zeroes */
	for(R_xlen_t i = 0; i < NRX*nry; i++) z[i].r = z[i].i = 0;
	return;
    }

#ifndef HAVE_FORTRAN_DOUBLE_COMPLEX
    if (R_Matprod == MATPROD_INTERNAL)
	internal_tccrossprod(x, nrx, ncx, y, nry, ncy, z);
    else
	simple_tccrossprod(x, nrx, ncx, y, nry, ncy, z);
#else
    switch(R_Matprod) {
	case MATPROD_DEFAULT:
	    if (cmayHaveNaNOrInf(x, NRX*ncx) || cmayHaveNaNOrInf(y, NRY*ncy)) {
		simple_tccrossprod(x, nrx, ncx, y, nry, ncy, z);
		return;
	    }
	    break; /* use blas */
	case MATPROD_INTERNAL:
	    internal_tccrossprod(x, nrx, ncx, y, nry, ncy, z);
	    return;
	case MATPROD_BLAS:
	    break;
	case MATPROD_DEFAULT_SIMD:
	    if (cmayHaveNaNOrInf_simd(x, NRX*ncx) ||
		    cmayHaveNaNOrInf_simd(y, NRY*ncy)) {
		simple_tccrossprod(x, nrx, ncx, y, nry, ncy, z);
		return;
	    }
	    break; /* use blas */
    }

    const char *transa = "N", *transb = "T";
    Rcomplex one, zero;

    one.r = 1.0; one.i = zero.r = zero.i = 0.0;

    F77_CALL(zgemm)(transa, transb, &nrx, &nry, &ncx, &one,
                    x, &nrx, y, &nry, &zero, z, &nrx);
#endif
}

/* "%*%" (op = 0), crossprod (op = 1) or tcrossprod (op = 2) */
SEXP do_crossprod(Expression* call, const BuiltInFunction* op, RObject* x, RObject* y)
{
    int ldx, ldy, nrx, ncx, nry, ncy;
    SEXPTYPE mode;
    SEXP xdims, ydims, ans;
    Rboolean sym;

    sym = Rf_isNull(y);
    if (sym && (op->variant() > 0)) y = x;
    if ( !(Rf_isNumeric(x) || Rf_isComplex(x)) || !(Rf_isNumeric(y) || Rf_isComplex(y)) )
	Rf_errorcall(call, _("requires numeric/complex matrix/vector arguments"));

    xdims = Rf_getAttrib(x, R_DimSymbol);
    ydims = Rf_getAttrib(y, R_DimSymbol);
    ldx = Rf_length(xdims);
    ldy = Rf_length(ydims);

    if (ldx != 2 && ldy != 2) {		/* x and y non-matrices */
	// for crossprod, allow two cases: n x n ==> (1,n) x (n,1);  1 x n = (n, 1) x (1, n)
	if (op->variant() == 1 && LENGTH(x) == 1) {
	    nrx = ncx = nry = 1;
	    ncy = LENGTH(y);
	}
	else {
	    nry = LENGTH(y);
	    ncy = 1;
	    if (op->variant() == 0) {
		nrx = 1;
		ncx = LENGTH(x);
		if(ncx == 1) {	        // y as row vector
		    ncy = nry;
		    nry = 1;
		}
	    }
	    else {
		nrx = LENGTH(x);
		ncx = 1;
	    }
	}
    }
    else if (ldx != 2) {		/* x not a matrix */
	nry = INTEGER(ydims)[0];
	ncy = INTEGER(ydims)[1];
	nrx = 0;
	ncx = 0;
	if (op->variant() == 0) {
	    if (LENGTH(x) == nry) {	/* x as row vector */
		nrx = 1;
		ncx = nry; /* == LENGTH(x) */
	    }
	    else if (nry == 1) {	/* x as col vector */
		nrx = LENGTH(x);
		ncx = 1;
	    }
	}
	else if (op->variant() == 1) { /* crossprod() */
	    if (LENGTH(x) == nry) {	/* x is a col vector */
		nrx = nry; /* == LENGTH(x) */
		ncx = 1;
	    }
	    /* else if (nry == 1) ... not being too tolerant
	       to treat x as row vector, as t(x) *is* row vector */
	}
	else { /* tcrossprod */
	    if (LENGTH(x) == ncy) {	/* x as row vector */
		nrx = 1;
		ncx = ncy; /* == LENGTH(x) */
	    }
	    else if (ncy == 1) {	/* x as col vector */
		nrx = LENGTH(x);
		ncx = 1;
	    }
	}
    }
    else if (ldy != 2) {		/* y not a matrix */
	nrx = INTEGER(xdims)[0];
	ncx = INTEGER(xdims)[1];
	nry = 0;
	ncy = 0;
	if (op->variant() == 0) {
	    if (LENGTH(y) == ncx) {	/* y as col vector */
		nry = ncx;
		ncy = 1;
	    }
	    else if (ncx == 1) {	/* y as row vector */
		nry = 1;
		ncy = LENGTH(y);
	    }
	}
	else if (op->variant() == 1) { /* crossprod() */
	    if (LENGTH(y) == nrx) {	/* y is a col vector */
		nry = nrx;
		ncy = 1;
	    } else if (nrx == 1) {	// y as row vector
		nry = 1;
		ncy = LENGTH(y);
	    }
	}
	else { // tcrossprod
	    if (nrx == 1) {		// y as row vector
		nry = 1;
		ncy = LENGTH(y);
	    }
	    else {			// y is a col vector
		nry = LENGTH(y);
		ncy = 1;
	    }
	}
    }
    else {				/* x and y matrices */
	nrx = INTEGER(xdims)[0];
	ncx = INTEGER(xdims)[1];
	nry = INTEGER(ydims)[0];
	ncy = INTEGER(ydims)[1];
    }
    /* nr[ow](.) and nc[ol](.) are now defined for x and y */

    if (op->variant() == 0) {
	/* primitive, so use call */
	if (ncx != nry)
	    Rf_errorcall(call, _("non-conformable arguments"));
    }
    else if (op->variant() == 1) {
	if (nrx != nry)
	    Rf_error(_("non-conformable arguments"));
    }
    else {
	if (ncx != ncy)
	    Rf_error(_("non-conformable arguments"));
    }

    if (Rf_isComplex(x) || Rf_isComplex(y))
	mode = CPLXSXP;
    else
	mode = REALSXP;
    x = Rf_coerceVector(x, mode);
    y = Rf_coerceVector(y, mode);

    if (op->variant() == 0) {			/* op == 0 : matprod() */

	PROTECT(ans = Rf_allocMatrix(mode, nrx, ncy));
	if (mode == CPLXSXP)
	    cmatprod(COMPLEX(x), nrx, ncx,
		     COMPLEX(y), nry, ncy, COMPLEX(ans));
	else
	    matprod(REAL(x), nrx, ncx,
		    REAL(y), nry, ncy, REAL(ans));

	PROTECT(xdims = Rf_getAttrib(x, R_DimNamesSymbol));
	PROTECT(ydims = Rf_getAttrib(y, R_DimNamesSymbol));

	if (xdims != R_NilValue || ydims != R_NilValue) {
	    SEXP dimnames, dimnamesnames, dnx=R_NilValue, dny=R_NilValue;

	    /* allocate dimnames and dimnamesnames */

	    PROTECT(dimnames = Rf_allocVector(VECSXP, 2));
	    PROTECT(dimnamesnames = Rf_allocVector(STRSXP, 2));
	    if (xdims != R_NilValue) {
		if (ldx == 2 || ncx == 1) {
		    SET_VECTOR_ELT(dimnames, 0, VECTOR_ELT(xdims, 0));
		    dnx = Rf_getAttrib(xdims, R_NamesSymbol);
		    if(!Rf_isNull(dnx))
			SET_STRING_ELT(dimnamesnames, 0, STRING_ELT(dnx, 0));
		}
	    }

#define YDIMS_ET_CETERA							\
	    if (ydims != R_NilValue) {					\
		if (ldy == 2) {						\
		    SET_VECTOR_ELT(dimnames, 1, VECTOR_ELT(ydims, 1));	\
		    dny = Rf_getAttrib(ydims, R_NamesSymbol);		\
		    if(!Rf_isNull(dny))					\
			SET_STRING_ELT(dimnamesnames, 1, STRING_ELT(dny, 1)); \
		} else if (nry == 1) {					\
		    SET_VECTOR_ELT(dimnames, 1, VECTOR_ELT(ydims, 0));	\
		    dny = Rf_getAttrib(ydims, R_NamesSymbol);		\
		    if(!Rf_isNull(dny))					\
			SET_STRING_ELT(dimnamesnames, 1, STRING_ELT(dny, 0)); \
		}							\
	    }								\
									\
	    /* We sometimes attach a dimnames attribute			\
	     * whose elements are all NULL ...				\
	     * This is ugly but causes no real damage.			\
	     * Now (2.1.0 ff), we don't anymore: */			\
	    if (VECTOR_ELT(dimnames,0) != R_NilValue ||			\
		VECTOR_ELT(dimnames,1) != R_NilValue) {			\
		if (dnx != R_NilValue || dny != R_NilValue)		\
		    Rf_setAttrib(dimnames, R_NamesSymbol, dimnamesnames);	\
		Rf_setAttrib(ans, R_DimNamesSymbol, dimnames);		\
	    }								\
	    UNPROTECT(2)

	    YDIMS_ET_CETERA;
	}
    }

    else if (op->variant() == 1) {	/* op == 1: crossprod() */

	PROTECT(ans = Rf_allocMatrix(mode, ncx, ncy));
	if (mode == CPLXSXP)
	    if(sym)
		ccrossprod(COMPLEX(x), nrx, ncx,
			   COMPLEX(x), nry, ncy, COMPLEX(ans));
	    else
		ccrossprod(COMPLEX(x), nrx, ncx,
			   COMPLEX(y), nry, ncy, COMPLEX(ans));
	else {
	    if(sym)
		symcrossprod(REAL(x), nrx, ncx, REAL(ans));
	    else
		crossprod(REAL(x), nrx, ncx,
			  REAL(y), nry, ncy, REAL(ans));
	}

	PROTECT(xdims = Rf_getAttrib(x, R_DimNamesSymbol));
	if (sym)
	    PROTECT(ydims = xdims);
	else
	    PROTECT(ydims = Rf_getAttrib(y, R_DimNamesSymbol));

	if (xdims != R_NilValue || ydims != R_NilValue) {
	    SEXP dimnames, dimnamesnames, dnx=R_NilValue, dny=R_NilValue;

	    /* allocate dimnames and dimnamesnames */

	    PROTECT(dimnames = Rf_allocVector(VECSXP, 2));
	    PROTECT(dimnamesnames = Rf_allocVector(STRSXP, 2));

	    if (xdims != R_NilValue) {
		if (ldx == 2) {/* not nrx==1 : .. fixed, ihaka 2003-09-30 */
		    SET_VECTOR_ELT(dimnames, 0, VECTOR_ELT(xdims, 1));
		    dnx = Rf_getAttrib(xdims, R_NamesSymbol);
		    if(!Rf_isNull(dnx))
			SET_STRING_ELT(dimnamesnames, 0, STRING_ELT(dnx, 1));
		}
	    }

	    YDIMS_ET_CETERA;
	}

    }
    else {					/* op == 2: tcrossprod() */

	PROTECT(ans = Rf_allocMatrix(mode, nrx, nry));
	if (mode == CPLXSXP)
	    if(sym)
		tccrossprod(COMPLEX(x), nrx, ncx,
			    COMPLEX(x), nry, ncy, COMPLEX(ans));
	    else
		tccrossprod(COMPLEX(x), nrx, ncx,
			    COMPLEX(y), nry, ncy, COMPLEX(ans));
	else {
	    if(sym)
		symtcrossprod(REAL(x), nrx, ncx, REAL(ans));
	    else
		tcrossprod(REAL(x), nrx, ncx,
			   REAL(y), nry, ncy, REAL(ans));
	}

	PROTECT(xdims = Rf_getAttrib(x, R_DimNamesSymbol));
	if (sym)
	    PROTECT(ydims = xdims);
	else
	    PROTECT(ydims = Rf_getAttrib(y, R_DimNamesSymbol));

	if (xdims != R_NilValue || ydims != R_NilValue) {
	    SEXP dimnames, dimnamesnames, dnx=R_NilValue, dny=R_NilValue;

	    /* allocate dimnames and dimnamesnames */

	    PROTECT(dimnames = Rf_allocVector(VECSXP, 2));
	    PROTECT(dimnamesnames = Rf_allocVector(STRSXP, 2));

	    if (xdims != R_NilValue) {
		if (ldx == 2) {
		    SET_VECTOR_ELT(dimnames, 0, VECTOR_ELT(xdims, 0));
		    dnx = Rf_getAttrib(xdims, R_NamesSymbol);
		    if(!Rf_isNull(dnx))
			SET_STRING_ELT(dimnamesnames, 0, STRING_ELT(dnx, 0));
		}
	    }
	    if (ydims != R_NilValue) {
		if (ldy == 2) {
		    SET_VECTOR_ELT(dimnames, 1, VECTOR_ELT(ydims, 0));
		    dny = Rf_getAttrib(ydims, R_NamesSymbol);
		    if(!Rf_isNull(dny))
			SET_STRING_ELT(dimnamesnames, 1, STRING_ELT(dny, 0));
		}
	    }
	    if (VECTOR_ELT(dimnames,0) != R_NilValue ||
		VECTOR_ELT(dimnames,1) != R_NilValue) {
		if (dnx != R_NilValue || dny != R_NilValue)
		    Rf_setAttrib(dimnames, R_NamesSymbol, dimnamesnames);
		Rf_setAttrib(ans, R_DimNamesSymbol, dimnames);
	    }

	    UNPROTECT(2);
	}
    }
    UNPROTECT(3);
    return ans;
}
#undef YDIMS_ET_CETERA

SEXP attribute_hidden do_matprod(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x = CAR(args), y = CADR(args);

    if ((IS_S4_OBJECT(x) || IS_S4_OBJECT(y))
	&& R_has_methods(SEXP_downcast<BuiltInFunction*>(op))) {
	SEXP s;
	/* Remove argument names to ensure positional matching */
	for(s = args; s != R_NilValue; s = CDR(s)) SET_TAG(s, R_NilValue);

	ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::EVALUATED);
	std::pair<bool, SEXP> pr
	    = R_possible_dispatch(SEXP_downcast<Expression*>(call),
				  SEXP_downcast<BuiltInFunction*>(op),
				  arglist,
				  SEXP_downcast<Environment*>(rho));
	if (pr.first) return pr.second;
    }
    return do_crossprod(SEXP_downcast<Expression*>(call),
			SEXP_downcast<BuiltInFunction*>(op), x, y);
}

SEXP attribute_hidden do_transpose(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_)
{
    SEXP a, r, dims, dimnames, dimnamesnames = R_NilValue,
	ndimnamesnames, rnames, cnames;
    int ldim, ncol = 0, nrow = 0;
    R_xlen_t len = 0;

    a = x_;

    if (Rf_isVector(a)) {
	dims = Rf_getAttrib(a, R_DimSymbol);
	ldim = Rf_length(dims);
	rnames = R_NilValue;
	cnames = R_NilValue;
	switch(ldim) {
	case 0:
	    len = nrow = LENGTH(a);
	    ncol = 1;
	    rnames = Rf_getAttrib(a, R_NamesSymbol);
	    dimnames = rnames;/* for Rf_isNull() below*/
	    break;
	case 1:
	    len = nrow = LENGTH(a);
	    ncol = 1;
	    dimnames = Rf_getAttrib(a, R_DimNamesSymbol);
	    if (dimnames != R_NilValue) {
		rnames = VECTOR_ELT(dimnames, 0);
		dimnamesnames = Rf_getAttrib(dimnames, R_NamesSymbol);
	    }
	    break;
	case 2:
	    ncol = Rf_ncols(a);
	    nrow = Rf_nrows(a);
	    len = XLENGTH(a);
	    dimnames = Rf_getAttrib(a, R_DimNamesSymbol);
	    if (dimnames != R_NilValue) {
		rnames = VECTOR_ELT(dimnames, 0);
		cnames = VECTOR_ELT(dimnames, 1);
		dimnamesnames = Rf_getAttrib(dimnames, R_NamesSymbol);
	    }
	    break;
	default:
	    Rf_error(_("argument is not a matrix"));
	}
    }
    else
	Rf_error(_("argument is not a matrix"));
    PROTECT(r = Rf_allocVector(TYPEOF(a), len));
    R_xlen_t i, j, l_1 = len-1;
    switch (TYPEOF(a)) {
    case LGLSXP:
    case INTSXP:
	// filling in columnwise, "accessing row-wise":
        for (i = 0, j = 0; i < len; i++, j += nrow) {
            if (j > l_1) j -= l_1;
            INTEGER(r)[i] = INTEGER(a)[j];
        }
        break;
    case REALSXP:
        for (i = 0, j = 0; i < len; i++, j += nrow) {
            if (j > l_1) j -= l_1;
            REAL(r)[i] = REAL(a)[j];
        }
        break;
    case CPLXSXP:
        for (i = 0, j = 0; i < len; i++, j += nrow) {
            if (j > l_1) j -= l_1;
            COMPLEX(r)[i] = COMPLEX(a)[j];
        }
        break;
    case STRSXP:
        for (i = 0, j = 0; i < len; i++, j += nrow) {
            if (j > l_1) j -= l_1;
            SET_STRING_ELT(r, i, STRING_ELT(a,j));
        }
        break;
    case VECSXP:
        for (i = 0, j = 0; i < len; i++, j += nrow) {
            if (j > l_1) j -= l_1;
            SET_VECTOR_ELT(r, i, VECTOR_ELT(a,j));
        }
        break;
    case RAWSXP:
        for (i = 0, j = 0; i < len; i++, j += nrow) {
            if (j > l_1) j -= l_1;
            RAW(r)[i] = RAW(a)[j];
        }
        break;
    default:
        UNPROTECT(1);
        Rf_error(_("argument is not a matrix"));
    }
    PROTECT(dims = Rf_allocVector(INTSXP, 2));
    INTEGER(dims)[0] = ncol;
    INTEGER(dims)[1] = nrow;
    Rf_setAttrib(r, R_DimSymbol, dims);
    UNPROTECT(1);
    /* R <= 2.2.0: dropped list(NULL,NULL) dimnames :
     * if(rnames != R_NilValue || cnames != R_NilValue) */
    if(!Rf_isNull(dimnames)) {
	PROTECT(dimnames = Rf_allocVector(VECSXP, 2));
	SET_VECTOR_ELT(dimnames, 0, cnames);
	SET_VECTOR_ELT(dimnames, 1, rnames);
	if(!Rf_isNull(dimnamesnames)) {
	    PROTECT(ndimnamesnames = Rf_allocVector(VECSXP, 2));
	    SET_VECTOR_ELT(ndimnamesnames, 1, STRING_ELT(dimnamesnames, 0));
	    SET_VECTOR_ELT(ndimnamesnames, 0,
			   (ldim == 2) ? STRING_ELT(dimnamesnames, 1):
			   R_BlankString);
	    Rf_setAttrib(dimnames, R_NamesSymbol, ndimnamesnames);
	    UNPROTECT(1);
	}
	Rf_setAttrib(r, R_DimNamesSymbol, dimnames);
	UNPROTECT(1);
    }
    Rf_copyMostAttrib(a, r);
    UNPROTECT(1);
    return r;
}

/*
 New version of aperm, using strides for speed.
 Jonathan Rougier <J.C.Rougier@durham.ac.uk>

 v1.0 30.01.01

 M.Maechler : expanded	all ../include/Rdefines.h macros
 */

/* this increments iip and sets j using strides */

#define CLICKJ						\
    for (itmp = 0; itmp < n; itmp++)			\
	if (iip[itmp] == isr[itmp]-1) iip[itmp] = 0;	\
	else {						\
	    iip[itmp]++;				\
	    break;					\
	}						\
    for (lj = 0, itmp = 0; itmp < n; itmp++)	       	\
	lj += iip[itmp] * stride[itmp];

/* aperm (a, perm, resize = TRUE) */
SEXP attribute_hidden do_aperm(/*const*/ Expression* call, const BuiltInFunction* op, RObject* a_, RObject* perm_, RObject* resize_)
{
    SEXP a, perm, r, dimsa, dimsr, dna;
    int i, j, n, itmp;

    a = a_;
    if (!Rf_isArray(a))
	Rf_error(_("invalid first argument, must be an array"));

    PROTECT(dimsa = Rf_getAttrib(a, R_DimSymbol));
    n = LENGTH(dimsa);
    int *isa = INTEGER(dimsa);

    /* check the permutation */

    int *pp = static_cast<int *>(RHO_alloc(size_t(n), sizeof(int)));
    perm = perm_;
    if (Rf_length(perm) == 0) {
	for (i = 0; i < n; i++) pp[i] = n-1-i;
    } else {
	if (LENGTH(perm) != n)
	    Rf_error(_("'perm' is of wrong length %d (!= %d)"),
		  LENGTH(perm), n);
	if (Rf_isString(perm)) {
	    SEXP dna = Rf_getAttrib(a, R_DimNamesSymbol);
	    if (Rf_isNull(dna))
		Rf_error(_("'a' does not have named dimnames"));
	    SEXP dnna = Rf_getAttrib(dna, R_NamesSymbol);
	    if (Rf_isNull(dnna))
		Rf_error(_("'a' does not have named dimnames"));
	    for (i = 0; i < n; i++) {
		const char *thiss = Rf_translateChar(STRING_ELT(perm, i));
		for (j = 0; j < n; j++)
		    if (streql(Rf_translateChar(STRING_ELT(dnna, j)),
			       thiss)) {pp[i] = j; break;}
		if (j >= n)
		    Rf_error(_("'perm[%d]' does not match a dimension name"), i+1);
	    }
	} else {
	    PROTECT(perm = Rf_coerceVector(perm, INTSXP));
	    for (i = 0; i < n; i++) pp[i] = INTEGER(perm)[i] - 1;
	    UNPROTECT(1);
	}
    }

    R_xlen_t *iip = static_cast<R_xlen_t *>(RHO_alloc(size_t(n), sizeof(R_xlen_t)));
    for (i = 0; i < n; iip[i++] = 0);
    for (i = 0; i < n; i++)
	if (pp[i] >= 0 && pp[i] < n) iip[pp[i]]++;
	else Rf_error(_("value out of range in 'perm'"));
    for (i = 0; i < n; i++)
	if (iip[i] == 0) Rf_error(_("invalid '%s' argument"), "perm");

    /* create the stride object and permute */

    R_xlen_t *stride = static_cast<R_xlen_t *>(RHO_alloc(size_t(n), sizeof(R_xlen_t)));
    for (iip[0] = 1, i = 1; i<n; i++) iip[i] = iip[i-1] * isa[i-1];
    for (i = 0; i < n; i++) stride[i] = iip[pp[i]];

    /* also need to have the dimensions of r */

    PROTECT(dimsr = Rf_allocVector(INTSXP, n));
    int *isr = INTEGER(dimsr);
    for (i = 0; i < n; i++) isr[i] = isa[pp[i]];

    /* and away we go! iip will hold the incrementer */

    R_xlen_t len = XLENGTH(a);
    PROTECT(r = Rf_allocVector(TYPEOF(a), len));

    for (i = 0; i < n; iip[i++] = 0);

    R_xlen_t li, lj;
    switch (TYPEOF(a)) {

    case INTSXP:
	for (lj = 0, li = 0; li < len; li++) {
	    INTEGER(r)[li] = INTEGER(a)[lj];
	    CLICKJ;
	}
	break;

    case LGLSXP:
	for (lj = 0, li = 0; li < len; li++) {
	    LOGICAL(r)[li] = LOGICAL(a)[lj];
	    CLICKJ;
	}
	break;

    case REALSXP:
	for (lj = 0, li = 0; li < len; li++) {
	    REAL(r)[li] = REAL(a)[lj];
	    CLICKJ;
	}
	break;

    case CPLXSXP:
	for (lj = 0, li = 0; li < len; li++) {
	    COMPLEX(r)[li].r = COMPLEX(a)[lj].r;
	    COMPLEX(r)[li].i = COMPLEX(a)[lj].i;
	    CLICKJ;
	}
	break;

    case STRSXP:
	for (lj = 0, li = 0; li < len; li++) {
	    SET_STRING_ELT(r, li, STRING_ELT(a, lj));
	    CLICKJ;
	}
	break;

    case VECSXP:
	for (lj = 0, li = 0; li < len; li++) {
	    SET_VECTOR_ELT(r, li, VECTOR_ELT(a, lj));
	    CLICKJ;
	}
	break;

    case RAWSXP:
	for (lj = 0, li = 0; li < len; li++) {
	    RAW(r)[li] = RAW(a)[lj];
	    CLICKJ;
	}
	break;

    default:
	UNIMPLEMENTED_TYPE("aperm", a);
    }

    /* handle the resize */
    int resize = Rf_asLogical(resize_);
    if (resize == NA_LOGICAL) Rf_error(_("'resize' must be TRUE or FALSE"));

    /* and handle names(dim(.)) and the dimnames if any */
    if (resize) {
	SEXP nmdm = Rf_getAttrib(dimsa, R_NamesSymbol);
	if(nmdm != R_NilValue) { // dimsr needs correctly permuted names()
	    PROTECT(nmdm);
	    SEXP nm_dr = PROTECT(Rf_allocVector(STRSXP, n));
	    for (i = 0; i < n; i++) {
		SET_STRING_ELT(nm_dr, i, STRING_ELT(nmdm, pp[i]));
	    }
	    Rf_setAttrib(dimsr, R_NamesSymbol, nm_dr);
	    UNPROTECT(2);
	}
	Rf_setAttrib(r, R_DimSymbol, dimsr);

	PROTECT(dna = Rf_getAttrib(a, R_DimNamesSymbol));
	if (dna != R_NilValue) {
	    SEXP dnna, dnr, dnnr;

	    PROTECT(dnr  = Rf_allocVector(VECSXP, n));
	    PROTECT(dnna = Rf_getAttrib(dna, R_NamesSymbol));
	    if (dnna != R_NilValue) {
		PROTECT(dnnr = Rf_allocVector(STRSXP, n));
		for (i = 0; i < n; i++) {
		    SET_VECTOR_ELT(dnr, i, VECTOR_ELT(dna, pp[i]));
		    SET_STRING_ELT(dnnr, i, STRING_ELT(dnna, pp[i]));
		}
		Rf_setAttrib(dnr, R_NamesSymbol, dnnr);
		UNPROTECT(1);
	    } else {
		for (i = 0; i < n; i++)
		    SET_VECTOR_ELT(dnr, i, VECTOR_ELT(dna, pp[i]));
	    }
	    Rf_setAttrib(r, R_DimNamesSymbol, dnr);
	    UNPROTECT(2);
	}
	UNPROTECT(1);
    }
    else // !resize
	Rf_setAttrib(r, R_DimSymbol, dimsa);

    UNPROTECT(3); /* dimsa, r, dimsr */
    return r;
}

/* colSums(x, n, p, na.rm) and friends */
SEXP attribute_hidden do_colsum(/*const*/ Expression* call, const BuiltInFunction* op, RObject* X_, RObject* n_, RObject* p_, RObject* na_rm_)
{
    SEXP x, ans = R_NilValue;
    int type;
    Rboolean NaRm, keepNA;

    x = X_;
    R_xlen_t n = asVecSize(n_);
    R_xlen_t p = asVecSize(p_);
    NaRm = Rboolean(Rf_asLogical(na_rm_));
    if (n == NA_INTEGER || n < 0)
	Rf_error(_("invalid '%s' argument"), "n");
    if (p == NA_INTEGER || p < 0)
	Rf_error(_("invalid '%s' argument"), "p");
    if (NaRm == NA_LOGICAL) Rf_error(_("invalid '%s' argument"), "na.rm");
    keepNA = Rboolean(!NaRm);

    switch (type = TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP: break;
    default:
	Rf_error(_("'x' must be numeric"));
    }
    if (n * (double)p > XLENGTH(x))
    	Rf_error(_("'x' is too short")); /* PR#16367 */

    int OP = op->variant();
    if (OP == 0 || OP == 1) { /* columns */
	PROTECT(ans = Rf_allocVector(REALSXP, p));
#ifdef _OPENMP
	int nthreads;
	/* This gives a spurious -Wunused-but-set-variable error */
	if (R_num_math_threads > 0)
	    nthreads = R_num_math_threads;
	else
	    nthreads = 1; /* for now */
#pragma omp parallel for num_threads(nthreads) default(none) \
    firstprivate(x, ans, n, p, type, NaRm, keepNA, R_NaReal, R_NaInt, OP)
#endif
	for (R_xlen_t j = 0; j < p; j++) {
	    R_xlen_t  cnt = n, i;
	    LDOUBLE sum = 0.0;
	    switch (type) {
	    case REALSXP:
	    {
		double *rx = REAL(x) + R_xlen_t(n)*j;
		if (keepNA)
		    for (sum = 0., i = 0; i < n; i++) sum += *rx++;
		else {
		    for (cnt = 0, sum = 0., i = 0; i < n; i++, rx++)
			if (!ISNAN(*rx)) {cnt++; sum += *rx;}
			else if (keepNA) {sum = NA_REAL; break;} // unused
		}
		break;
	    }
	    case INTSXP:
	    {
		int *ix = INTEGER(x) + R_xlen_t(n)*j;
		for (cnt = 0, sum = 0., i = 0; i < n; i++, ix++)
		    if (*ix != NA_INTEGER) {cnt++; sum += *ix;}
		    else if (keepNA) {sum = NA_REAL; break;}
		break;
	    }
	    case LGLSXP:
	    {
		int *ix = LOGICAL(x) + R_xlen_t(n)*j;
		for (cnt = 0, sum = 0., i = 0; i < n; i++, ix++)
		    if (*ix != NA_LOGICAL) {cnt++; sum += *ix;}
		    else if (keepNA) {sum = NA_REAL; break;}
		break;
	    }
	    }
	    if (OP == 1) sum /= cnt; /* gives NaN for cnt = 0 */
	    REAL(ans)[j] = double(sum);
	}
    }
    else { /* rows */
	PROTECT(ans = Rf_allocVector(REALSXP, n));

	/* allocate scratch storage to allow accumulating by columns
	   to improve cache hits */
	int *Cnt = nullptr;
	LDOUBLE *rans;
	if(n <= 10000) {
	    R_CheckStack2(n * sizeof(LDOUBLE));
	    rans = static_cast<LDOUBLE *>(alloca(n * sizeof(LDOUBLE)));
	    Memzero(rans, n);
	} else rans = Calloc(n, LDOUBLE);
	if (!keepNA && OP == 3) Cnt = Calloc(n, int);

	for (R_xlen_t j = 0; j < p; j++) {
	    LDOUBLE *ra = rans;
	    switch (type) {
	    case REALSXP:
	    {
		double *rx = REAL(x) + R_xlen_t(n) * j;
		if (keepNA)
		    for (R_xlen_t i = 0; i < n; i++) *ra++ += *rx++;
		else
		    for (R_xlen_t i = 0; i < n; i++, ra++, rx++)
			if (!ISNAN(*rx)) {
			    *ra += *rx;
			    if (OP == 3) Cnt[i]++;
			}
		break;
	    }
	    case INTSXP:
	    {
		int *ix = INTEGER(x) + (R_xlen_t)n * j;
		for (R_xlen_t i = 0; i < n; i++, ra++, ix++)
		    if (keepNA) {
			if (*ix != NA_INTEGER) *ra += *ix;
			else *ra = NA_REAL;
		    }
		    else if (*ix != NA_INTEGER) {
			*ra += *ix;
			if (OP == 3) Cnt[i]++;
		    }
		break;
	    }
	    case LGLSXP:
	    {
		int *ix = LOGICAL(x) + (R_xlen_t)n * j;
		for (R_xlen_t i = 0; i < n; i++, ra++, ix++)
		    if (keepNA) {
			if (*ix != NA_LOGICAL) *ra += *ix;
			else *ra = NA_REAL;
		    }
		    else if (*ix != NA_LOGICAL) {
			*ra += *ix;
			if (OP == 3) Cnt[i]++;
		    }
		break;
	    }
	    }
	}
	if (OP == 3) {
	    if (keepNA)
		for (R_xlen_t i = 0; i < n; i++) rans[i] /= p;
	    else
		for (R_xlen_t i = 0; i < n; i++) rans[i] /= Cnt[i];
	}
	for (R_xlen_t i = 0; i < n; i++) REAL(ans)[i] = (double) rans[i];

	if (!keepNA && OP == 3) Free(Cnt);
	if(n > 10000) Free(rans);
    }

    UNPROTECT(1);
    return ans;
}

/*
{
    data <- as.vector(data)
    dim <- as.integer(dim)
    vl <- prod(dim)
    if (Rf_length(data) != vl) {
        if (vl > .Machine$integer.max)
            stop("'dim' specifies too large an array")
        data <- rep(data, length.out = vl)
    }
    if (Rf_length(dim))
        dim(data) <- dim
    if (is.list(dimnames) && Rf_length(dimnames))
        dimnames(data) <- dimnames
    data
}
*/

/* array(data, dim, dimnames) */
SEXP attribute_hidden do_array(/*const*/ Expression* call, const BuiltInFunction* op, RObject* data_, RObject* dim_, RObject* dimnames_)
{
    SEXP vals, ans, dims, dimnames;
    R_xlen_t lendat, i, nans;

    vals = data_;
    /* at least NULL can get here */
    switch(TYPEOF(vals)) {
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	case RAWSXP:
	case EXPRSXP:
	case VECSXP:
	    break;
	default:
	    Rf_error(_("'data' must be of a vector type, was '%s'"),
		Rf_type2char(TYPEOF(vals)));
    }
    lendat = XLENGTH(vals);
    dims = dim_;
    dimnames = dimnames_;
    PROTECT(dims = Rf_coerceVector(dims, INTSXP));
    int nd = LENGTH(dims);
    if (nd == 0) Rf_error(_("'dims' cannot be of length 0"));
    double d = 1.0;
    for (int j = 0; j < nd; j++) d *= INTEGER(dims)[j];
#ifndef LONG_VECTOR_SUPPORT
    if (d > INT_MAX) Rf_error(_("too many elements specified"));
#endif
    nans = R_xlen_t(d);

    PROTECT(ans = Rf_allocVector(TYPEOF(vals), nans));
    switch(TYPEOF(vals)) {
    case LGLSXP:
	if (nans && lendat)
	    xcopyLogicalWithRecycle(LOGICAL(ans), LOGICAL(vals), 0, nans,
				    lendat);
	else
	    for (i = 0; i < nans; i++) LOGICAL(ans)[i] = NA_LOGICAL;
	break;
    case INTSXP:
	if (nans && lendat)
	    xcopyIntegerWithRecycle(INTEGER(ans), INTEGER(vals), 0, nans,
				    lendat);
	else
	    for (i = 0; i < nans; i++) INTEGER(ans)[i] = NA_INTEGER;
	break;
    case REALSXP:
	if (nans && lendat)
	    xcopyRealWithRecycle(REAL(ans), REAL(vals), 0, nans, lendat);
	else
	    for (i = 0; i < nans; i++) REAL(ans)[i] = NA_REAL;
	break;
    case CPLXSXP:
	if (nans && lendat)
	    xcopyComplexWithRecycle(COMPLEX(ans), COMPLEX(vals), 0, nans,
				    lendat);
	else {
	    Rcomplex na_cmplx;
	    na_cmplx.r = NA_REAL;
	    na_cmplx.i = 0;
	    for (i = 0; i < nans; i++) COMPLEX(ans)[i] = na_cmplx;
	}
	break;
    case RAWSXP:
	if (nans && lendat)
	    xcopyRawWithRecycle(RAW(ans), RAW(vals), 0, nans, lendat);
	else
	    for (i = 0; i < nans; i++) RAW(ans)[i] = 0;
	break;
    case STRSXP:
	if (nans && lendat)
	    xcopyStringWithRecycle(ans, vals, 0, nans, lendat);
	else
	    for (i = 0; i < nans; i++) SET_STRING_ELT(ans, i, NA_STRING);
	break;
    /* Rest are already initialized */
    case VECSXP:
    case EXPRSXP:
#ifdef SWITCH_TO_REFCNT
	if (nans && lendat)
	    xcopyVectorWithRecycle(ans, vals, 0, nans, lendat);
#else
	if (nans && lendat) {
	    /* Need to guard against possible sharing of values under
	       NAMED.  This is not needed with reference
	       coutning. (PR#15919) */
	    Rboolean needsmark = Rboolean(lendat < nans
					  || MAYBE_REFERENCED(vals));
	    for (i = 0; i < nans; i++) {
		SEXP elt = VECTOR_ELT(vals, i % lendat);
		if (needsmark || MAYBE_REFERENCED(elt))
		    MARK_NOT_MUTABLE(elt);
		SET_VECTOR_ELT(ans, i, elt);
	    }
	}
#endif
	break;
    default:
	// excluded above
	break;
    }

    ans = Rf_dimgets(ans, dims);
    if(!Rf_isNull(dimnames) && Rf_length(dimnames) > 0)
	ans = Rf_dimnamesgets(ans, dimnames);

    UNPROTECT(2);
    return ans;
}

SEXP attribute_hidden do_diag(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* nrow_, RObject* ncol_)
{
    SEXP ans, x, snr, snc;
    int nr = 1, nc = 1, nprotect = 1;

    x = x_;
    snr = nrow_;
    snc = ncol_;
    nr = Rf_asInteger(snr);
    if (nr == NA_INTEGER)
	Rf_error(_("invalid 'nrow' value (too large or NA)"));
    if (nr < 0)
	Rf_error(_("invalid 'nrow' value (< 0)"));
    nc = Rf_asInteger(snc);
    if (nc == NA_INTEGER)
	Rf_error(_("invalid 'ncol' value (too large or NA)"));
    if (nc < 0)
	Rf_error(_("invalid 'ncol' value (< 0)"));
    int mn = (nr < nc) ? nr : nc;
    if (mn > 0 && Rf_length(x) == 0)
	Rf_error(_("'x' must have positive length"));

#ifndef LONG_VECTOR_SUPPORT
   if ((double)nr * (double)nc > INT_MAX)
	Rf_error(_("too many elements specified"));
#endif

   int nx = LENGTH(x);
   R_xlen_t NR = nr;

#define mk_DIAG(_zero_)					\
   for (R_xlen_t i = 0; i < NR*nc; i++) ra[i] = _zero_;	\
   R_xlen_t i, i1;					\
   MOD_ITERATE1(mn, nx, i, i1, {			\
	   ra[i * (NR+1)] = rx[i1];			\
   });

   switch(TYPEOF(x)) {

   case REALSXP:
   {
#define mk_REAL_DIAG					\
       PROTECT(ans = Rf_allocMatrix(REALSXP, nr, nc));	\
       double *rx = REAL(x), *ra = REAL(ans);		\
       mk_DIAG(0.0)

       mk_REAL_DIAG;
       break;
   }
   case CPLXSXP:
   {
       PROTECT(ans = Rf_allocMatrix(CPLXSXP, nr, nc));
       int nx = LENGTH(x);
       R_xlen_t NR = nr;
       Rcomplex *rx = COMPLEX(x), *ra = COMPLEX(ans), zero;
       zero.r = zero.i = 0.0;
       mk_DIAG(zero);
       break;
   }
   case INTSXP:
   {
       PROTECT(ans = Rf_allocMatrix(INTSXP, nr, nc));
       int *rx = INTEGER(x), *ra = INTEGER(ans);
       mk_DIAG(0);
       break;
   }
   case LGLSXP:
   {
       PROTECT(ans = Rf_allocMatrix(LGLSXP, nr, nc));
       int *rx = LOGICAL(x), *ra = LOGICAL(ans);
       mk_DIAG(0);
       break;
   }
   case RAWSXP:
   {
       PROTECT(ans = Rf_allocMatrix(RAWSXP, nr, nc));
       Rbyte *rx = RAW(x), *ra = RAW(ans);
       mk_DIAG((Rbyte) 0);
       break;
   }
   default: {
       PROTECT(x = Rf_coerceVector(x, REALSXP));
       nprotect++;
       mk_REAL_DIAG;
     }
   }
#undef mk_REAL_DIAG
#undef mk_DIAG
   UNPROTECT(nprotect);
   return ans;
}


/* backsolve(r, b, k, upper.tri, transpose) */
SEXP attribute_hidden do_backsolve(/*const*/ Expression* call, const BuiltInFunction* op, RObject* r_, RObject* x_, RObject* k_, RObject* upper_tri_, RObject* transpose_)
{
    int nprot = 1;

    SEXP r = r_;
    SEXP b = x_;
    int nrr = Rf_nrows(r), nrb = Rf_nrows(b), ncb = Rf_ncols(b);
    int k = Rf_asInteger(k_);
    /* k is the number of rows to be used: there must be at least that
       many rows and cols in the rhs and at least that many rows on
       the rhs.
    */
    if (k == NA_INTEGER || k <= 0 || k > nrr || k > Rf_ncols(r) || k > nrb)
	Rf_error(_("invalid '%s' argument"), "k");
    int upper = Rf_asLogical(upper_tri_);
    if (upper == NA_INTEGER) Rf_error(_("invalid '%s' argument"), "upper.tri");
    int trans = Rf_asLogical(transpose_);
    if (trans == NA_INTEGER) Rf_error(_("invalid '%s' argument"), "transpose");
    if (TYPEOF(r) != REALSXP) {PROTECT(r = Rf_coerceVector(r, REALSXP)); nprot++;}
    if (TYPEOF(b) != REALSXP) {PROTECT(b = Rf_coerceVector(b, REALSXP)); nprot++;}
    double *rr = REAL(r);

    /* check for zeros on diagonal of r: only k row/cols are used. */
    size_t incr = nrr + 1;
    for(int i = 0; i < k; i++) { /* check for zeros on diagonal */
	if (rr[i * incr] == 0.0)
	    Rf_error(_("singular matrix in 'backsolve'. First zero in diagonal [%d]"),
		  i + 1);
    }

    SEXP ans = PROTECT(Rf_allocMatrix(REALSXP, k, ncb));
    if (k > 0 && ncb > 0) {
       /* copy (part) cols of b to ans */
	for(R_xlen_t j = 0; j < ncb; j++)
	    memcpy(REAL(ans) + j*k, REAL(b) + j*nrb, size_t(k) *sizeof(double));
	double one = 1.0;
	F77_CALL(dtrsm)("L", upper ? "U" : "L", trans ? "T" : "N", "N",
			&k, &ncb, &one, rr, &nrr, REAL(ans), &k);
    }
    UNPROTECT(nprot);
    return ans;
}

/* max.col(m, ties.method) */
SEXP attribute_hidden do_maxcol(/*const*/ Expression* call, const BuiltInFunction* op, RObject* m_, RObject* ties_method_)
{
    SEXP m = m_;
    int method = Rf_asInteger(ties_method_);
    int nr = Rf_nrows(m), nc = Rf_ncols(m), nprot = 1;
    if (TYPEOF(m) != REALSXP) {PROTECT(m = Rf_coerceVector(m, REALSXP)); nprot++;}
    SEXP ans = PROTECT(Rf_allocVector(INTSXP, nr));
    R_max_col(REAL(m), &nr, &nc, INTEGER(ans), &method);
    UNPROTECT(nprot);
    return ans;
}
