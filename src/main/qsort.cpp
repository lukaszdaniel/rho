/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2002-2017   The R Core Team.
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

/* **********************************************************************
 * === This was 'sort()' in  gamfit's  mysort.f  [or sortdi() in sortdi.f ] :
 * was at end of  modreg/src/ppr.f
 * Translated by f2c (version 20010821) and f2c-clean,v 1.9 2000/01/13 13:46:53
 * then manually by Martin Maechler
*/

#define R_NO_REMAP

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <R_ext/RS.h>
#include <Defn.h> /* => Utils.h with the protos from here */
#include <Localization.h>
#include <Internal.h>
#include <Rmath.h>

#ifdef LONG_VECTOR_SUPPORT
static void R_qsort_R(double *v, double *I, size_t i, size_t j);
static void R_qsort_int_R(int *v, double *I, size_t i, size_t j);
#endif

/* R function  qsort(x, index.return) */
SEXP attribute_hidden do_qsort(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* index_return_)
{
    SEXP x, sx;
    int indx_ret;
    double *vx = nullptr;
    int *ivx = nullptr;
    Rboolean x_real, x_int;

    x = x_;
    if (!Rf_isNumeric(x))
	Rf_error(_("argument is not a numeric vector"));
    x_real= Rboolean(TYPEOF(x) == REALSXP);
    x_int = Rboolean(!x_real && (TYPEOF(x) == INTSXP || TYPEOF(x) == LGLSXP));
    PROTECT(sx = (x_real || x_int) ? Rf_duplicate(x) : Rf_coerceVector(x, REALSXP));
    sx->clearAttributes();
    indx_ret = Rf_asLogical(index_return_);
    R_xlen_t n = XLENGTH(x);
#ifdef LONG_VECTOR_SUPPORT
    Rboolean isLong = Rboolean(n > INT_MAX);
#endif
    if(x_int) ivx = INTEGER(sx); else vx = REAL(sx);
    if(indx_ret) {
	SEXP ans, ansnames, indx;
	/* answer will have x = sorted x , ix = index :*/
	PROTECT(ans = Rf_allocVector(VECSXP, 2));
	PROTECT(ansnames = Rf_allocVector(STRSXP, 2));
#ifdef LONG_VECTOR_SUPPORT
	if (isLong) {
	    PROTECT(indx = Rf_allocVector(REALSXP, n));
	    double *ix = REAL(indx);
	    for(R_xlen_t i = 0; i < n; i++) ix[i] = double((i+1));
	    if(x_int) R_qsort_int_R(ivx, ix, 1, n);
	    else R_qsort_R(vx, ix, 1, n);
	} else
#endif
	{
	    PROTECT(indx = Rf_allocVector(INTSXP, n));
	    int *ix = INTEGER(indx);
	    int nn = int(n);
	    for(int i = 0; i < nn; i++) ix[i] = i+1;
	    if(x_int) R_qsort_int_I(ivx, ix, 1, nn);
	    else R_qsort_I(vx, ix, 1, nn);
	}
	SET_VECTOR_ELT(ans, 0, sx);
	SET_VECTOR_ELT(ans, 1, indx);
	SET_STRING_ELT(ansnames, 0, Rf_mkChar("x"));
	SET_STRING_ELT(ansnames, 1, Rf_mkChar("ix"));
	Rf_setAttrib(ans, R_NamesSymbol, ansnames);
	UNPROTECT(4);
	return ans;
    } else {
	if(x_int)
	    R_qsort_int(ivx, 1, n);
	else
	    R_qsort(vx, 1, n);

	UNPROTECT(1);
	return sx;
    }
}


/* These are exposed in Utils.h and are misguidely in the API */
void F77_SUB(qsort4)(double *v, int *indx, int *ii, int *jj)
{
    R_qsort_I(v, indx, *ii, *jj);
}

void F77_SUB(qsort3)(double *v, int *ii, int *jj)
{
    R_qsort(v, *ii, *jj);
}

//  sort with index : --------------------------
#define qsort_Index
#define INTt int
#define INDt int

#define NUMERIC double
void R_qsort_I(double *v, int *I, int i, int j)
#include "qsort-body.cpp"
#undef NUMERIC

#define NUMERIC int
void R_qsort_int_I(int *v, int *I, int i, int j)
#include "qsort-body.cpp"
#undef NUMERIC

#undef INTt
#undef INDt

#ifdef LONG_VECTOR_SUPPORT
#define INDt double
#define NUMERIC double
static void R_qsort_R(double *v, double *I, size_t i, size_t j)
#include "qsort-body.cpp"
#undef NUMERIC

#define NUMERIC int
static void R_qsort_int_R(int *v, double *I, size_t i, size_t j)
#include "qsort-body.cpp"
#undef NUMERIC
#undef INDt
#endif // LONG_VECTOR_SUPPORT

//  sort withOUT index : -----------------------
#undef qsort_Index

#define NUMERIC double
void R_qsort(double *v, size_t i, size_t j)
#include "qsort-body.cpp"
#undef NUMERIC

#define NUMERIC int
void R_qsort_int(int *v, size_t i, size_t j)
#include "qsort-body.cpp"
#undef NUMERIC
