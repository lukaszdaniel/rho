/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2016  the R Core Team
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

#include <math.h> // for isfinite
#include <Rinternals.h>
#include <R_ext/Applic.h>

#include "localization.h"


/* called via .External(.) :*/
SEXP call_dqags(SEXP args);
SEXP call_dqagi(SEXP args);

typedef struct int_struct
{
    SEXP f;    /* function */
    SEXP env;  /* where to evaluate the calls */
} int_struct, *IntStruct;


/* This is *the* ``integr_fn f'' used when called from R : */
static void Rintfn(double *x, int n, void *ex)
{
    SEXP args, resultsxp, tmp;
    int i;
    IntStruct IS = (IntStruct) ex;

    PROTECT(args = Rf_allocVector(REALSXP, n));
    for(i = 0; i < n; i++) REAL(args)[i] = x[i];

    PROTECT(tmp = Rf_lang2(IS->f , args));
    PROTECT(resultsxp = Rf_eval(tmp, IS->env));

    if(Rf_length(resultsxp) != n)
	Rf_error("evaluation of function gave a result of wrong length");
    if(TYPEOF(resultsxp) == INTSXP) {
	resultsxp = Rf_coerceVector(resultsxp, REALSXP);
    } else if(TYPEOF(resultsxp) != REALSXP)
	Rf_error("evaluation of function gave a result of wrong type");
    for(i = 0; i < n; i++) {
	x[i] = REAL(resultsxp)[i];
	if(!R_FINITE(x[i]))
	    Rf_error("non-finite function value");
    }
    UNPROTECT(3);
    return;
}

SEXP call_dqags(SEXP args)
{
    int_struct is;
    SEXP ans, ansnames;
    double lower, upper, epsabs, epsrel, result, abserr, *work;
    int neval, ier, limit, lenw, last, *iwork;

    args = CDR(args);
    is.f = CAR(args); args = CDR(args);
    is.env = CAR(args); args = CDR(args);
    if(Rf_length(CAR(args)) > 1) Rf_error(_("'%s' must be of length one"), "lower");
    lower = Rf_asReal(CAR(args)); args = CDR(args);
    if(Rf_length(CAR(args)) > 1) Rf_error(_("'%s' must be of length one"), "upper");
    upper = Rf_asReal(CAR(args)); args = CDR(args);
    epsabs = Rf_asReal(CAR(args)); args = CDR(args);
    epsrel = Rf_asReal(CAR(args)); args = CDR(args);
    limit = Rf_asInteger(CAR(args)); args = CDR(args);
    lenw = 4 * limit;
    iwork = (int *) R_alloc((size_t) limit, sizeof(int));
    work = (double *) R_alloc((size_t) lenw, sizeof(double));

    Rdqags(Rintfn, (void*)&is,
	   &lower, &upper, &epsabs, &epsrel, &result,
	   &abserr, &neval, &ier, &limit, &lenw, &last, iwork, work);

    PROTECT(ans = Rf_allocVector(VECSXP, 4));
    PROTECT(ansnames = Rf_allocVector(STRSXP, 4));
    SET_STRING_ELT(ansnames, 0, Rf_mkChar("value"));
    SET_VECTOR_ELT(ans, 0, Rf_allocVector(REALSXP, 1));
    REAL(VECTOR_ELT(ans, 0))[0] = result;
    SET_STRING_ELT(ansnames, 1, Rf_mkChar("abs.error"));
    SET_VECTOR_ELT(ans, 1, Rf_allocVector(REALSXP, 1));
    REAL(VECTOR_ELT(ans, 1))[0] = abserr;
    SET_STRING_ELT(ansnames, 2, Rf_mkChar("subdivisions"));
    SET_VECTOR_ELT(ans, 2, Rf_allocVector(INTSXP, 1));
    INTEGER(VECTOR_ELT(ans, 2))[0] = last;
    SET_STRING_ELT(ansnames, 3, Rf_mkChar("ierr"));
    SET_VECTOR_ELT(ans, 3, Rf_allocVector(INTSXP, 1));
    INTEGER(VECTOR_ELT(ans, 3))[0] = ier;
    Rf_setAttrib(ans, R_NamesSymbol, ansnames);
    UNPROTECT(2);
    return ans;
}

SEXP call_dqagi(SEXP args)
{
    int_struct is;
    SEXP ans, ansnames;
    double bound, epsabs, epsrel, result, abserr, *work;
    int inf, neval, ier, limit, lenw, last, *iwork;

    args = CDR(args);
    is.f = CAR(args); args = CDR(args);
    is.env = CAR(args); args = CDR(args);
    if(Rf_length(CAR(args)) > 1) Rf_error(_("'%s' must be of length one"), "bound");
    bound = Rf_asReal(CAR(args)); args = CDR(args);
    inf = Rf_asInteger(CAR(args)); args = CDR(args);
    epsabs = Rf_asReal(CAR(args)); args = CDR(args);
    epsrel = Rf_asReal(CAR(args)); args = CDR(args);
    limit = Rf_asInteger(CAR(args)); args = CDR(args);
    lenw = 4 * limit;
    iwork = (int *) R_alloc((size_t) limit, sizeof(int));
    work = (double *) R_alloc((size_t) lenw, sizeof(double));

    Rdqagi(Rintfn, (void*)&is, &bound,&inf,&epsabs,&epsrel,&result,
	   &abserr,&neval,&ier,&limit,&lenw,&last,iwork,work);

    PROTECT(ans = Rf_allocVector(VECSXP, 4));
    PROTECT(ansnames = Rf_allocVector(STRSXP, 4));
    SET_STRING_ELT(ansnames, 0, Rf_mkChar("value"));
    SET_VECTOR_ELT(ans, 0, Rf_allocVector(REALSXP, 1));
    REAL(VECTOR_ELT(ans, 0))[0] = result;
    SET_STRING_ELT(ansnames, 1, Rf_mkChar("abs.error"));
    SET_VECTOR_ELT(ans, 1, Rf_allocVector(REALSXP, 1));
    REAL(VECTOR_ELT(ans, 1))[0] = abserr;
    SET_STRING_ELT(ansnames, 2, Rf_mkChar("subdivisions"));
    SET_VECTOR_ELT(ans, 2, Rf_allocVector(INTSXP, 1));
    INTEGER(VECTOR_ELT(ans, 2))[0] = last;
    SET_STRING_ELT(ansnames, 3, Rf_mkChar("ierr"));
    SET_VECTOR_ELT(ans, 3, Rf_allocVector(INTSXP, 1));
    INTEGER(VECTOR_ELT(ans, 3))[0] = ier;
    Rf_setAttrib(ans, R_NamesSymbol, ansnames);
    UNPROTECT(2);
    return ans;
}
