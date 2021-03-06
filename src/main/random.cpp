/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2015  The R Core Team
 *  Copyright (C) 2003--2008  The R Foundation
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
# include <config.h>
#endif

#include <Defn.h>
#include <Localization.h>

#include <R_ext/Random.h>
#include <R_ext/RS.h>		/* for Calloc() */
#include <Rmath.h>		/* for rxxx functions */
#include "basedecl.h"
#include <errno.h>
#include <Internal.h>
#include <rho/GCStackRoot.hpp>
#include <rho/RAllocStack.hpp>

using namespace rho;

/*
 *  Unequal Probability Sampling.
 *
 *  Modelled after Fortran code provided by:
 *    E. S. Venkatraman <venkat@biosta.mskcc.org>
 *  but with significant modifications in the
 *  "with replacement" case.
 */

/* Unequal probability sampling; with-replacement case */

static void ProbSampleReplace(int n, double *p, int *perm, int nans, int *ans)
{
    double rU;
    int i, j;
    int nm1 = n - 1;

    /* record element identities */
    for (i = 0; i < n; i++)
	perm[i] = i + 1;

    /* sort the probabilities into descending order */
    Rf_revsort(p, perm, n);

    /* compute cumulative probabilities */
    for (i = 1 ; i < n; i++)
	p[i] += p[i - 1];

    /* compute the sample */
    for (i = 0; i < nans; i++) {
	rU = unif_rand();
	for (j = 0; j < nm1; j++) {
	    if (rU <= p[j])
		break;
	}
	ans[i] = perm[j];
    }
}

/* A  version using Walker's alias method, based on Alg 3.13B in
   Ripley (1987).
 */

constexpr int SMALL = 10000;
static void walker_ProbSampleReplace(int n, double *p, int *a, int nans, int *ans)
{
    double *q, rU;
    int i, j, k;
    int *HL, *H, *L;

    /* Create the alias tables.
       The idea is that for HL[0] ... L-1 label the entries with q < 1
       and L ... H[n-1] label those >= 1.
       By rounding error we could have q[i] < 1. or > 1. for all entries.
     */
    if(n <= SMALL) {
	R_CheckStack2(n *(sizeof(int) + sizeof(double)));
	/* might do this repeatedly, so speed matters */
	HL = static_cast<int *>(alloca(n * sizeof(int)));
	q = static_cast<double *>(alloca(n * sizeof(double)));
    } else {
	/* Slow enough anyway not to risk overflow */
	HL = Calloc(n, int);
	q = Calloc(n, double);
    }
    H = HL - 1; L = HL + n;
    for (i = 0; i < n; i++) {
	q[i] = p[i] * n;
	if (q[i] < 1.) *++H = i; else *--L = i;
    }
    if (H >= HL && L < HL + n) { /* So some q[i] are >= 1 and some < 1 */
	for (k = 0; k < n - 1; k++) {
	    i = HL[k];
	    j = *L;
	    a[i] = j;
	    q[j] += q[i] - 1;
	    if (q[j] < 1.) L++;
	    if(L >= HL + n) break; /* now all are >= 1 */
	}
    }
    for (i = 0; i < n; i++) q[i] += i;

    /* generate sample */
    for (i = 0; i < nans; i++) {
	rU = unif_rand() * n;
	k = int(rU);
	ans[i] = (rU < q[k]) ? k+1 : a[k]+1;
    }
    if(n > SMALL) {
	Free(HL);
	Free(q);
    }
}


/* Unequal probability sampling; without-replacement case */

static void ProbSampleNoReplace(int n, double *p, int *perm,
				int nans, int *ans)
{
    double rT, mass, totalmass;
    int i, j, k, n1;

    /* Record element identities */
    for (i = 0; i < n; i++)
	perm[i] = i + 1;

    /* Sort probabilities into descending order */
    /* Order element identities in parallel */
    Rf_revsort(p, perm, n);

    /* Compute the sample */
    totalmass = 1;
    for (i = 0, n1 = n-1; i < nans; i++, n1--) {
	rT = totalmass * unif_rand();
	mass = 0;
	for (j = 0; j < n1; j++) {
	    mass += p[j];
	    if (rT <= mass)
		break;
	}
	ans[i] = perm[j];
	totalmass -= p[j];
	for(k = j; k < n1; k++) {
	    p[k] = p[k + 1];
	    perm[k] = perm[k + 1];
	}
    }
}

static void FixupProb(double *p, int n, int require_k, Rboolean replace)
{
    double sum = 0.0;
    int npos = 0;
    for (int i = 0; i < n; i++) {
	if (!std::isfinite(p[i]))
	    Rf_error(_("NA in probability vector"));
	if (p[i] < 0.0)
	    Rf_error(_("negative probability"));
	if (p[i] > 0.0) {
	    npos++;
	    sum += p[i];
	}
    }
    if (npos == 0 || (!replace && require_k > npos))
	Rf_error(_("too few positive probabilities"));
    for (int i = 0; i < n; i++) p[i] /= sum;
}

/* do_sample - probability sampling with/without replacement.
   .Internal(sample(n, size, replace, prob))
*/
HIDDEN RObject* do_sample(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* size_, RObject* replace_, RObject* prob_)
{
    SEXP x, y, sn, sk, prob, sreplace;

    sn = x_;
    sk = size_;
    sreplace = replace_;
    if(Rf_length(sreplace) != 1)
	 Rf_error(_("invalid '%s' argument"), "replace");
    int replace = Rf_asLogical(sreplace);
    prob = prob_;
    if (replace == R_NaLog)
	Rf_error(_("invalid '%s' argument"), "replace");
    GetRNGstate();
    if (!Rf_isNull(prob)) {
	int n = Rf_asInteger(sn), k = Rf_asInteger(sk);
	if (n == R_NaInt || n < 0 || (k > 0 && n == 0))
	    Rf_error(_("invalid first argument"));
	if (k == R_NaInt || k < 0)
	    Rf_error(_("invalid '%s' argument"), "size");
	if (!replace && k > n)
	    Rf_error(_("cannot take a sample larger than the population when 'replace = FALSE'"));
	PROTECT(y = Rf_allocVector(INTSXP, k));
	prob = Rf_coerceVector(prob, REALSXP);
	if (MAYBE_REFERENCED(prob)) prob = Rf_duplicate(prob);
	PROTECT(prob);
	double *p = REAL(prob);
	if (Rf_length(prob) != n)
	    Rf_error(_("incorrect number of probabilities"));
	FixupProb(p, n, k, Rboolean(replace));
	PROTECT(x = Rf_allocVector(INTSXP, n));
	if (replace) {
	    int i, nc = 0;
	    for (i = 0; i < n; i++) if(n * p[i] > 0.1) nc++;
	    if (nc > 200)
		walker_ProbSampleReplace(n, p, INTEGER(x), k, INTEGER(y));
	    else
		ProbSampleReplace(n, p, INTEGER(x), k, INTEGER(y));
	} else
	    ProbSampleNoReplace(n, p, INTEGER(x), k, INTEGER(y));
	UNPROTECT(2);
    }
    else {  // uniform sampling
	double dn = Rf_asReal(sn);
	R_xlen_t k = asVecSize(sk);
	if (!std::isfinite(dn) || dn < 0 || dn > 4.5e15 || (k > 0 && dn == 0))
	    Rf_error(_("invalid first argument"));
	if (k < 0) Rf_error(_("invalid '%s' argument"), "size");
	if (!replace && k > dn)
	    Rf_error(_("cannot take a sample larger than the population when 'replace = FALSE'"));
	if (dn > INT_MAX || k > INT_MAX) {
	    PROTECT(y = Rf_allocVector(REALSXP, k));
	    if (replace) {
		double *ry = REAL(y);
		for (R_xlen_t i = 0; i < k; i++) ry[i] = R_unif_index(dn) + 1;
	    } else {
#ifdef LONG_VECTOR_SUPPORT
		R_xlen_t n = R_xlen_t(dn);
		double *x = static_cast<double *>(RHO_alloc(n, sizeof(double)));
		double *ry = REAL(y);
		for (R_xlen_t i = 0; i < n; i++) x[i] = double(i);
		for (R_xlen_t i = 0; i < k; i++) {
		    R_xlen_t j = (R_xlen_t) R_unif_index(n);
		    ry[i] = x[j] + 1;
		    x[j] = x[--n];
		}
#else
		Rf_error(_("n >= 2^31, replace = FALSE is only supported on 64-bit platforms"));
#endif
	    }
	} else {
	    int n = int(dn);
	    PROTECT(y = Rf_allocVector(INTSXP, k));
	    int *iy = INTEGER(y);
	    /* avoid allocation for a single sample */
	    if (replace || k < 2) {
		for (int i = 0; i < k; i++) iy[i] = (int)(R_unif_index(dn) + 1);
	    } else {
		int *x = static_cast<int *>(RHO_alloc(n, sizeof(int)));
		for (int i = 0; i < n; i++) x[i] = i;
		for (int i = 0; i < k; i++) {
		    int j = (int)(R_unif_index(n));
		    iy[i] = x[j] + 1;
		    x[j] = x[--n];
		}
	    }
	}
    }
    PutRNGstate();
    UNPROTECT(1);
    return y;
}
