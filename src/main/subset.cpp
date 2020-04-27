/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-2015   The R Core Team
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
 *
 *
 *  Vector and List Subsetting
 *
 *  There are three kinds of subscripting [, [[, and $.
 *  We have three different functions to compute these.
 *
 *
 *  Note on Matrix Subscripts
 *
 *  The special [ subscripting where dim(x) == ncol(subscript matrix)
 *  is handled inside VectorSubset. The subscript matrix is turned
 *  into a subscript vector of the appropriate size and then
 *  VectorSubset continues.  This provides coherence especially
 *  regarding attributes etc. (it would be quicker to handle this case
 *  separately, but then we would have more to keep in step.
 */

#define R_NO_REMAP

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdexcept>
#include <Defn.h>
#include <Internal.h>
#include "rho/BuiltInFunction.hpp"
#include "rho/ComplexVector.hpp"
#include "rho/ExpressionVector.hpp"
#include "rho/GCStackRoot.hpp"
#include "rho/Promise.hpp"
#include "rho/RawVector.hpp"
#include "rho/Subscripting.hpp"

using namespace std;
using namespace rho;

/* JMC convinced MM that this was not a good idea: */
#undef _S4_subsettable


static R_INLINE SEXP VECTOR_ELT_FIX_NAMED(SEXP y, R_xlen_t i) {
    /* if RHS (container or element) has NAMED > 0 set NAMED = NAMEDMAX.
       Duplicating might be safer/more consistent (fix bug reported by
       Radford Neal; similar to PR15098) */
    SEXP val = VECTOR_ELT(y, i);
    if ((NAMED(y) || NAMED(val)))
	ENSURE_NAMEDMAX(val);
    return val;
}

static R_INLINE SEXP XVECTOR_ELT_FIX_NAMED(SEXP y, R_xlen_t i) {
    /* if RHS (container or element) has NAMED > 0 set NAMED = NAMEDMAX.
       Duplicating might be safer/more consistent (fix bug reported by
       Radford Neal; similar to PR15098) */
    SEXP val = XVECTOR_ELT(y, i);
    if ((NAMED(y) || NAMED(val)))
	ENSURE_NAMEDMAX(val);
    return val;
}

/* ExtractSubset allocates "result" and does the transfer of elements
   from "x" to "result" according to the integer/real subscripts given
   in "indx".

   The EXTRACT_SUBSET_LOOP macro allows the branches based on index
   type and vector type to happen outside the loop.

   This could avoid using data pointers, but there is little point as
   currently the subscript code forces allocation.
*/

#define EXTRACT_SUBSET_LOOP(STDCODE, NACODE) do { \
	if (TYPEOF(indx) == INTSXP) {		  \
	    int *pindx = INTEGER(indx);		  \
	    for (i = 0; i < n; i++) {		  \
		ii = pindx[i];			  \
		if (0 < ii && ii <= nx) {	  \
		    ii--;			  \
		    STDCODE;			  \
		}				  \
		else /* out of bounds or NA */	  \
		    NACODE;			  \
	    }					  \
	}					  \
	else {					  \
	    double *pindx = REAL(indx);		  \
	    for (i = 0; i < n; i++) {		  \
		double di = pindx[i];		  \
		ii = (R_xlen_t) (di - 1);	  \
		if (R_FINITE(di) &&		  \
		    0 <= ii && ii < nx)		  \
		    STDCODE;			  \
		else				  \
		    NACODE;			  \
	    }					  \
	}					  \
    } while (0)
    
SEXP attribute_hidden Rf_ExtractSubset(SEXP x, SEXP indx, SEXP call)
{
    if (x == R_NilValue)
	return x;

    SEXP result;

    if (ALTREP(x)) {
	result = ALTVEC_EXTRACT_SUBSET(x, indx, call);
	if (result != NULL)
	    return result;
    }

    R_xlen_t i, ii, n, nx;
    n = XLENGTH(indx);
    nx = Rf_xlength(x);
    SEXPTYPE mode = TYPEOF(x);

    /* protect allocation in case _ELT operations need to allocate */
    PROTECT(result = Rf_allocVector(mode, n));
    switch(mode) {
    case LGLSXP:
	EXTRACT_SUBSET_LOOP(LOGICAL0(result)[i] = Rboolean(LOGICAL_ELT(x, ii)),
			    LOGICAL0(result)[i] = Rboolean(NA_INTEGER));
	break;
    case INTSXP:
	EXTRACT_SUBSET_LOOP(INTEGER0(result)[i] = INTEGER_ELT(x, ii),
			    INTEGER0(result)[i] = NA_INTEGER);
	break;
    case REALSXP:
	EXTRACT_SUBSET_LOOP(REAL0(result)[i] = REAL_ELT(x, ii),
			    REAL0(result)[i] = NA_REAL);
	break;
    case CPLXSXP:
	{
	    Rcomplex NA_CPLX = { NA_REAL, NA_REAL };
	    EXTRACT_SUBSET_LOOP(COMPLEX0(result)[i] = COMPLEX_ELT(x, ii),
				COMPLEX0(result)[i] = NA_CPLX);
	}
	break;
    case STRSXP:
	EXTRACT_SUBSET_LOOP(SET_STRING_ELT(result, i, STRING_ELT(x, ii)),
			    SET_STRING_ELT(result, i, NA_STRING));
	break;
    case VECSXP:
	EXTRACT_SUBSET_LOOP(SET_VECTOR_ELT(result, i,
					   VECTOR_ELT_FIX_NAMED(x, ii)),
			    SET_VECTOR_ELT(result, i, R_NilValue));
	break;
    case EXPRSXP:
	EXTRACT_SUBSET_LOOP(SET_XVECTOR_ELT(result, i,
					   XVECTOR_ELT_FIX_NAMED(x, ii)),
			    SET_XVECTOR_ELT(result, i, R_NilValue));
	break;
    case RAWSXP:
	EXTRACT_SUBSET_LOOP(RAW0(result)[i] = RAW_ELT(x, ii),
			    RAW0(result)[i] = (Rbyte) 0);
	break;
    case LISTSXP:
	/* cannot happen: pairlists are coerced to lists */
    case LANGSXP:
	/* cannot happen: LANGSXPs are coerced to lists */
    default:
	Rf_errorcall(call, _("object of type '%s' is not subsettable"), Rf_type2char(SEXPTYPE(mode)));
    }
    UNPROTECT(1); /* result */
    return result;
}


/* This is for all cases with a single index, including 1D arrays and
   matrix indexing of arrays */
static SEXP VectorSubset(SEXP x, SEXP sarg, SEXP call)
{
    if (!x)
	return nullptr;
    GCStackRoot<> s(sarg);

    if (s == R_MissingArg)
	return Rf_duplicate(x);

    /* Check to see if we have special matrix subscripting. */
    /* If we do, make a real subscript vector and protect it. */
    {
	SEXP attrib = Rf_getAttrib(x, R_DimSymbol);	
	if (Rf_isMatrix(s) && Rf_isArray(x) && Rf_ncols(s) == Rf_length(attrib)) {
	    if (Rf_isString(s)) {
		s = Rf_strmat2intmat(s, Rf_GetArrayDimnames(x), call);
	    }
	    if (Rf_isInteger(s) || Rf_isReal(s)) {
		s = Rf_mat2indsub(attrib, s, call);
	    }
	}
    }

    SEXPTYPE mode = TYPEOF(x);
    switch (mode) {
    case LGLSXP:
	return Subscripting::vectorSubset(static_cast<LogicalVector*>(x), s);
    case INTSXP:
	return Subscripting::vectorSubset(static_cast<IntVector*>(x), s);
    case REALSXP:
	return Subscripting::vectorSubset(static_cast<RealVector*>(x), s);
    case CPLXSXP:
	return Subscripting::vectorSubset(static_cast<ComplexVector*>(x), s);
    case RAWSXP:
	return Subscripting::vectorSubset(static_cast<RawVector*>(x), s);
    case STRSXP:
	return Subscripting::vectorSubset(static_cast<StringVector*>(x), s);
    case VECSXP:
	return Subscripting::vectorSubset(static_cast<ListVector*>(x), s);
    case EXPRSXP:
	return Subscripting::vectorSubset(static_cast<ExpressionVector*>(x), s);
    case LANGSXP:
	break;
    default:
	Rf_errorcall(call, _("object of type '%s' is not subsettable"), Rf_type2char(SEXPTYPE(mode)));
    }

    // If we get to here, this must be a LANGSXP.  In rho, this case
    // needs special handling, not least because Expression doesn't
    // inherit from VectorBase.  What follows is legacy CR code,
    // bodged as necessary.

    /* Convert to a vector of integer subscripts */
    /* in the range 1:length(x). */
    R_xlen_t stretch = 1;
    GCStackRoot<> indx(Rf_makeSubscript(x, s, &stretch, call));
    GCStackRoot<> result(Rf_ExtractSubset(x, indx, call));
    if (mode == VECSXP || mode == EXPRSXP)
	/* we do not duplicate the values when extracting the subset,
	   so to be conservative mark the result as NAMED = NAMEDMAX */
	ENSURE_NAMEDMAX(result);

    // Fix attributes:
    {
	SEXP attrib;
	if (
	    ((attrib = Rf_getAttrib(x, R_NamesSymbol)) != R_NilValue) ||
	    ( /* here we might have an array.  Use row names if 1D */
	     Rf_isArray(x)
	     && (attrib = Rf_getAttrib(x, R_DimNamesSymbol)) != R_NilValue
	     && Rf_length(attrib) == 1
	     && (attrib = Rf_GetRowNames(attrib)) != R_NilValue
	     )
	    ) {
	    GCStackRoot<> nattrib(Rf_ExtractSubset(attrib, indx, call));
	    Rf_setAttrib(result, R_NamesSymbol, nattrib);
	}
	if ((attrib = Rf_getAttrib(x, R_SrcrefSymbol)) != R_NilValue &&
	    TYPEOF(attrib) == VECSXP) {
	    GCStackRoot<> nattrib(Rf_ExtractSubset(attrib, indx, call));
	    Rf_setAttrib(result, R_SrcrefSymbol, nattrib);
	}
    }
    return result;
}


static SEXP ArraySubset(SEXP x, SEXP s, SEXP call, int drop)
{
    const PairList* subs = SEXP_downcast<PairList*>(s);
    switch (x->sexptype()) {
    case LGLSXP:
	return Subscripting::arraySubset(static_cast<LogicalVector*>(x),
					 subs, drop);
    case INTSXP:
	return Subscripting::arraySubset(static_cast<IntVector*>(x),
					 subs, drop);
    case REALSXP:
	return Subscripting::arraySubset(static_cast<RealVector*>(x),
					 subs, drop);
    case CPLXSXP:
	return Subscripting::arraySubset(static_cast<ComplexVector*>(x),
					 subs, drop);
    case STRSXP:
	return Subscripting::arraySubset(static_cast<StringVector*>(x),
					 subs, drop);
    case VECSXP:
	return Subscripting::arraySubset(static_cast<ListVector*>(x),
					 subs, drop);
    case RAWSXP:
	return Subscripting::arraySubset(static_cast<RawVector*>(x),
					 subs, drop);
    default:
	Rf_errorcall(call, _("array subscripting not handled for this type"));
    }
    return nullptr;  // -Wall
}


/* Returns and removes a named argument from argument list args.
   The search ends as soon as a matching argument is found.  If
   the argument is not found, the argument list is not modified
   and R_NilValue is returned.
 */
static SEXP ExtractArg(SEXP args, SEXP arg_sym)
{
    SEXP arg, prev_arg;
    int found = 0;

    for (arg = prev_arg = args; arg != R_NilValue; arg = CDR(arg)) {
	if(TAG(arg) == arg_sym) {
	    if (arg == prev_arg) /* found at head of args */
		args = CDR(args);
	    else
		SETCDR(prev_arg, CDR(arg));
	    found = 1;
	    break;
	}
	else  prev_arg = arg;
    }
    return found ? CAR(arg) : R_NilValue;
}

/* Extracts the drop argument, if present, from the argument list.
   The object being subsetted must be the first argument. */
static void ExtractDropArg(SEXP el, int *drop)
{
    SEXP dropArg = ExtractArg(el, R_DropSymbol);
    *drop = Rf_asLogical(dropArg);
    if (*drop == NA_LOGICAL) *drop = 1;
}


/* Extracts and, if present, removes the 'exact' argument from the
   argument list.  An integer code giving the desired exact matching
   behavior is returned:
       0  not exact
       1  exact
      -1  not exact, but warn when partial matching is used
 */
static int ExtractExactArg(SEXP args)
{
    SEXP argval = ExtractArg(args, R_ExactSymbol);
    int exact;
    if(Rf_isNull(argval)) return 1; /* Default is true as from R 2.7.0 */
    exact = Rf_asLogical(argval);
    if (exact == NA_LOGICAL) exact = -1;
    return exact;
}

/* The "[" subset operator. */
SEXP attribute_hidden do_subset(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    const Expression* expression = SEXP_downcast<Expression*>(call);
    const BuiltInFunction* function = SEXP_downcast<BuiltInFunction*>(op);
    Environment* envx = SEXP_downcast<Environment*>(rho);
    ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::RAW);
    auto dispatched = Rf_DispatchOrEval(expression, function, &arglist, envx,
                                       MissingArgHandling::Keep);
    if (dispatched.first) {
        RObject* ans = dispatched.second;
	if (NAMED(ans))
	    ENSURE_NAMEDMAX(ans);
	return(ans);
    }

    /* Method dispatch has failed, we now */
    /* run the generic internal code. */
    return BuiltInFunction::callBuiltInWithCApi(
        do_subset_dflt, expression, function, arglist, envx);
}

static R_INLINE R_xlen_t scalarIndex(SEXP s)
{
    if (ATTRIB(s) == R_NilValue)
	switch (TYPEOF(s)) {
	case INTSXP:
	{
	    int ival = SCALAR_IVAL(s);
	    if (XLENGTH(s) == 1 && ival != NA_INTEGER)
		return ival;
	    else return -1;
	}
	case REALSXP:
	{
	    double rval = SCALAR_DVAL(s);
	    // treat infinite indices as NA, like asInteger
	    if (XLENGTH(s) == 1 && R_FINITE(rval))
		return R_xlen_t(rval);
	    else return -1;
	}
	default: return -1;
	}
    else return -1;
}

SEXP attribute_hidden do_subset_dflt(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    GCStackRoot<> argsrt(args);

    /* By default we drop extents of length 1 */

    /* Handle cases of extracting a single element from a simple vector
       or matrix directly to improve speed for these simple cases. */
    SEXP cdrArgs = CDR(args);
    SEXP cddrArgs = CDR(cdrArgs);
    if (cdrArgs != R_NilValue && cddrArgs == R_NilValue &&
	TAG(cdrArgs) == R_NilValue) {
	/* one index, not named */
	SEXP x = CAR(args);
	if (ATTRIB(x) == R_NilValue) {
	    SEXP s = CAR(cdrArgs);
	    R_xlen_t i = scalarIndex(s);
	    switch (TYPEOF(x)) {
	    case REALSXP:
		if (i >= 1 && i <= XLENGTH(x))
		    return Rf_ScalarReal( REAL_ELT(x, i-1) );
		break;
	    case INTSXP:
		if (i >= 1 && i <= XLENGTH(x))
		    return Rf_ScalarInteger( INTEGER_ELT(x, i-1) );
		break;
	    case LGLSXP:
		if (i >= 1 && i <= XLENGTH(x))
		    return Rf_ScalarLogical( LOGICAL_ELT(x, i-1) );
		break;
//	    do the more rare cases as well, since we've already prepared everything:
	    case CPLXSXP:
		if (i >= 1 && i <= XLENGTH(x))
		    return Rf_ScalarComplex( COMPLEX_ELT(x, i-1) );
		break;
	    case RAWSXP:
		if (i >= 1 && i <= XLENGTH(x))
		    return Rf_ScalarRaw( RAW_ELT(x, i-1) );
		break;
	    default: break;
	    }
	}
    }
    else if (cddrArgs != R_NilValue && CDR(cddrArgs) == R_NilValue &&
	     TAG(cdrArgs) == R_NilValue && TAG(cddrArgs) == R_NilValue) {
	/* two indices, not named */
	SEXP x = CAR(args);
	SEXP attr = ATTRIB(x);
	if (TAG(attr) == R_DimSymbol && CDR(attr) == R_NilValue) {
	    /* only attribute of x is 'dim' */
	    SEXP dim = CAR(attr);
	    if (TYPEOF(dim) == INTSXP && LENGTH(dim) == 2) {
		/* x is a matrix */
		SEXP si = CAR(cdrArgs);
		SEXP sj = CAR(cddrArgs);
		R_xlen_t i = scalarIndex(si);
		R_xlen_t j = scalarIndex(sj);
		int nrow = INTEGER_ELT(dim, 0);
		int ncol = INTEGER_ELT(dim, 1);
		if (i > 0 && j > 0 && i <= nrow && j <= ncol) {
		    /* indices are legal scalars */
		    R_xlen_t k = i - 1 + nrow * (j - 1);
		    switch (TYPEOF(x)) {
		    case REALSXP:
			if (k < XLENGTH(x))
			    return Rf_ScalarReal( REAL_ELT(x, k) );
			break;
		    case INTSXP:
			if (k < XLENGTH(x))
			    return Rf_ScalarInteger( INTEGER_ELT(x, k) );
			break;
		    case LGLSXP:
			if (k < XLENGTH(x))
			    return Rf_ScalarLogical( LOGICAL_ELT(x, k) );
			break;
		    case CPLXSXP:
			if (k < XLENGTH(x))
			    return Rf_ScalarComplex( COMPLEX_ELT(x, k) );
			break;
		    case RAWSXP:
			if (k < XLENGTH(x))
			    return Rf_ScalarRaw( RAW_ELT(x, k) );
			break;
		    default: break;
		    }
		}
	    }
	}
    }

    int drop = 1;
    ExtractDropArg(args, &drop);
    SEXP x = CAR(args);

    /* This was intended for compatibility with S, */
    /* but in fact S does not do this. */
    /* FIXME: replace the test by isNull ... ? */

    if (x == R_NilValue) {
	return x;
    }
    SEXP subs = CDR(args);
    size_t nsubs = Rf_length(subs);

    /* Here coerce pair-based objects into generic vectors. */
    /* All subsetting takes place on the generic vector form. */

    GCStackRoot<> ax(x);
    if (Rf_isPairList(x)) {
	SEXP dim = Rf_getAttrib(x, R_DimSymbol);
	int ndim = Rf_length(dim);
	if (ndim > 1) {
	    ax = Rf_allocArray(VECSXP, dim);
	    Rf_setAttrib(ax, R_DimNamesSymbol, Rf_getAttrib(x, R_DimNamesSymbol));
	    Rf_setAttrib(ax, R_NamesSymbol, Rf_getAttrib(x, R_DimNamesSymbol));
	}
	else {
	    ax = Rf_allocVector(VECSXP, Rf_length(x));
	    Rf_setAttrib(ax, R_NamesSymbol, Rf_getAttrib(x, R_NamesSymbol));
	}
	int i = 0;
	for (SEXP px = x; px != R_NilValue; px = CDR(px))
	    SET_VECTOR_ELT(ax, i++, CAR(px));
    }
    else if (!Rf_isVector(x))
	Rf_errorcall(call, _("object of type '%s' is not subsettable"), Rf_type2char(TYPEOF(x)));

    /* This is the actual subsetting code. */

    GCStackRoot<> ans;
    SEXP sub1 = CAR(subs);  // null if nsubs == 0
    if (nsubs == 1 && sub1 == R_MissingArg) { // Handle x[] correctly.
	nsubs = 0;
	sub1 = R_NilValue;
    }

    const IntVector* dims = static_cast<VectorBase*>(ax.get())->dimensions();
    if (dims) {
	size_t ndim = dims->size();
	// Check for single matrix subscript:
	if (nsubs == 1 && Rf_isMatrix(sub1)
	    && Rf_isArray(ax) && Rf_ncols(sub1) == int(ndim))
	    ans = VectorSubset(ax, sub1, call);
	else if (ndim == nsubs)  // regular array subscripting, inc. 1-dim
	    ans = ArraySubset(ax, subs, call, drop);
    }
    if (!ans) {
	if (nsubs < 2) // vector subscripting
	    ans = VectorSubset(ax, (nsubs == 1 ? sub1 : R_MissingArg), call);
	else Rf_errorcall(call, _("incorrect number of dimensions"));
    }

    /* Note: we do not coerce back to pair-based lists. */
    /* They are "defunct" in this version of R. */

    if (TYPEOF(x) == LANGSXP) {
	ax = ans;
	ans = nullptr;
	size_t len = LENGTH(ax);
	if (len > 0) {
	    GCStackRoot<PairList> tl(PairList::make(len - 1));
	    ans = new CachingExpression(nullptr, tl);

	    int i = 0;
	    for (SEXP px = ans; px != R_NilValue; px = CDR(px))
		SETCAR(px, VECTOR_ELT(ax, i++));
	    Rf_setAttrib(ans, R_DimSymbol, Rf_getAttrib(ax, R_DimSymbol));
	    Rf_setAttrib(
		ans, R_DimNamesSymbol, Rf_getAttrib(ax, R_DimNamesSymbol));
	    Rf_setAttrib(ans, R_NamesSymbol, Rf_getAttrib(ax, R_NamesSymbol));
	    SET_NAMED(ans, NAMED(ax)); /* PR#7924 */
	}
    }
    if (ATTRIB(ans) != R_NilValue) { /* remove probably erroneous attr's */
	Rf_setAttrib(ans, R_TspSymbol, R_NilValue);
#ifdef _S4_subsettable
	if(!IS_S4_OBJECT(x))
#endif
	    Rf_setAttrib(ans, R_ClassSymbol, R_NilValue);
    }
    return ans;
}

/* The [[ subset operator.  It needs to be fast. */
/* The arguments to this call are evaluated on entry. */

SEXP attribute_hidden do_subset2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    const Expression* expression = SEXP_downcast<Expression*>(call);
    const BuiltInFunction* function = SEXP_downcast<BuiltInFunction*>(op);
    Environment* envx = SEXP_downcast<Environment*>(rho);
    ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::RAW);
    auto dispatched = Rf_DispatchOrEval(expression, function, &arglist, envx,
                                        MissingArgHandling::Keep);
    if (dispatched.first) {
        SEXP ans = dispatched.second;
	if (NAMED(ans))
	    ENSURE_NAMEDMAX(ans);
	return(ans);
    }

    return BuiltInFunction::callBuiltInWithCApi(
        do_subset2_dflt, expression, function, arglist, envx);
}

SEXP attribute_hidden do_subset2_dflt(SEXP call, SEXP op,
				      SEXP argsarg, SEXP rho)
{
    GCStackRoot<> args(argsarg);
    SEXP ans;
    int drop = 1;
    ExtractDropArg(args, &drop);
    /* Is partial matching ok?  When the exact arg is NA, a warning is
       issued if partial matching occurs.
     */
    int exact = ExtractExactArg(args);
    int pok;
    if (exact == -1)
	pok = exact;
    else
	pok = !exact;

    SEXP x = CAR(args);

    /* This code was intended for compatibility with S, */
    /* but in fact S does not do this.	Will anyone notice? */
    if (!x)
	return nullptr;

    /* Get the subscripting and dimensioning information */
    /* and check that any array subscripting is compatible. */

    SEXP subs = CDR(args);
    int nsubs = Rf_length(subs);
    if (nsubs == 0)
	Rf_errorcall(call, _("no index specified"));
    SEXP dims = Rf_getAttrib(x, R_DimSymbol);
    int ndims = Rf_length(dims);
    if (nsubs > 1 && nsubs != ndims)
	Rf_errorcall(call, _("incorrect number of subscripts"));

    /* code to allow classes to extend environment */
    if (TYPEOF(x) == S4SXP) {
	x = R_getS4DataSlot(x, ANYSXP);
	if (x == R_NilValue)
	  Rf_errorcall(call, _("this S4 class is not subsettable"));
    }

    /* split out ENVSXP for now */
    if ( TYPEOF(x) == ENVSXP ) {
	if( nsubs != 1 || !Rf_isString(CAR(subs)) || Rf_length(CAR(subs)) != 1 )
	    Rf_errorcall(call, _("wrong arguments for subsetting an environment"));
	GCStackRoot<>
	    ans(Rf_findVarInFrame(x,
			       Rf_install(Rf_translateChar(STRING_ELT(CAR(subs),
								0)))));
	if ( TYPEOF(ans) == PROMSXP )
	    ans = Rf_eval(ans, R_GlobalEnv);
	else ENSURE_NAMEDMAX(ans);

	if (ans == R_UnboundValue )
	    return(R_NilValue);
	if (NAMED(ans))
	    ENSURE_NAMEDMAX(ans);
	return ans;
    }

    /* back to the regular program */
    if (!(Rf_isVector(x) || Rf_isList(x) || Rf_isLanguage(x)))
	Rf_errorcall(call, _("object of type '%s' is not subsettable"), Rf_type2char(TYPEOF(x)));

    int named_x = NAMED(x);  /* x may change below; save this now.  See PR#13411 */

    R_xlen_t offset = 0;
    if (nsubs == 1) { /* vector indexing */
	SEXP thesub = CAR(subs);
	int len = Rf_length(thesub);

	if (len > 1) {
#ifdef SWITCH_TO_REFCNT
	    if (IS_GETTER_CALL(call)) {
		/* this is (most likely) a getter call in a complex
		   assighment so we duplicate as needed. The original
		   x should have been duplicated if it might be
		   shared */
		if (MAYBE_SHARED(x))
		    Rf_error("getter call used outside of a complex assignment.");
		x = vectorIndex(x, thesub, 0, len-1, pok, call, TRUE);
	    }
	    else
		x = vectorIndex(x, thesub, 0, len-1, pok, call, FALSE);
#else
	    x = vectorIndex(x, thesub, 0, len-1, pok, call, FALSE);
#endif
	    named_x = NAMED(x);
	}

	offset = Rf_get1index(thesub, Rf_getAttrib(x, R_NamesSymbol),
			   Rf_xlength(x), pok, len > 1 ? len-1 : -1, call);
	if (offset < 0 || offset >= Rf_xlength(x)) {
	    /* a bold attempt to get the same behaviour for $ and [[ */
	    if (offset < 0 && (Rf_isNewList(x) ||
			       Rf_isExpression(x) ||
			       Rf_isList(x) ||
			       Rf_isLanguage(x)))
		return nullptr;
	    else Rf_errorcall(call, _("subscript out of bounds"));
	}
    } else { /* matrix indexing */
	/* Here we use the fact that: */
	/* CAR(R_NilValue) = R_NilValue */
	/* CDR(R_NilValue) = R_NilValue */

	int ndn; /* Number of dimnames. Unlikely to be anything but
		    0 or nsubs, but just in case... */

	GCStackRoot<> indx(Rf_allocVector(INTSXP, nsubs));
	int *pindx = INTEGER(indx);
	int *pdims = INTEGER(dims);
	SEXP dimnames = Rf_getAttrib(x, R_DimNamesSymbol);
	ndn = Rf_length(dimnames);
	for (int i = 0; i < nsubs; i++) {
	    pindx[i] = int(
		Rf_get1index(CAR(subs),
			  (i < ndn) ? VECTOR_ELT(dimnames, i) : R_NilValue,
			  pindx[i], pok, -1, call));
	    subs = CDR(subs);
	    if (pindx[i] < 0 || pindx[i] >= pdims[i])
		Rf_errorcall(call, _("subscript out of bounds"));
	}
	offset = 0;
	for (int i = (nsubs - 1); i > 0; i--)
	    offset = (offset + pindx[i]) * pdims[i - 1];
	offset += pindx[0];
    }

    if (Rf_isPairList(x)) {
#ifdef LONG_VECTOR_SUPPORT
	if (offset > R_SHORT_LEN_MAX)
	    Rf_error("invalid subscript for pairlist");
#endif
	ans = CAR(Rf_nthcdr(x, int(offset)));
	if (named_x > NAMED(ans))
	    SET_NAMED(ans, named_x);
    } else if (Rf_isVectorList(x)) {
	/* did unconditional duplication before 2.4.0 */
	if (x->sexptype() == EXPRSXP)
	    ans = XVECTOR_ELT(x, offset);
	else ans = VECTOR_ELT(x, offset);
	if (named_x > NAMED(ans))
	    SET_NAMED(ans, named_x);
    } else {
	ans = Rf_allocVector(TYPEOF(x), 1);
	switch (TYPEOF(x)) {
	case LGLSXP:
	    LOGICAL0(ans)[0] = Rboolean(LOGICAL_ELT(x, offset));
	    break;
	case INTSXP:
	    INTEGER0(ans)[0] = INTEGER_ELT(x, offset);
	    break;
	case REALSXP:
	    REAL0(ans)[0] = REAL_ELT(x, offset);
	    break;
	case CPLXSXP:
	    COMPLEX0(ans)[0] = COMPLEX_ELT(x, offset);
	    break;
	case STRSXP:
	    SET_STRING_ELT(ans, 0, STRING_ELT(x, offset));
	    break;
	case RAWSXP:
	    RAW0(ans)[0] = RAW_ELT(x, offset);
	    break;
	default:
	    UNIMPLEMENTED_TYPE("do_subset2", x);
	}
    }
    return ans;
}

SEXP attribute_hidden dispatch_subset2(SEXP x, R_xlen_t i, SEXP call, SEXP rho)
{
    static SEXP bracket_op = NULL;
    SEXP args, x_elt;
    if (Rf_isObject(x)) {
        if (bracket_op == NULL)
            bracket_op = R_Primitive("[[");
        PROTECT(args = Rf_list2(x, Rf_ScalarReal(i + 1)));
        x_elt = do_subset2(call, bracket_op, args, rho);
        UNPROTECT(1);
    } else {
      // FIXME: throw error if not a list
        x_elt = VECTOR_ELT(x, i);
    }
    return(x_elt);
}

enum pmatch {
    NO_MATCH,
    EXACT_MATCH,
    PARTIAL_MATCH
};

/* A helper to partially match tags against a candidate.
   Tags are always in the native charset.
 */
/* Returns: */
static
enum pmatch
pstrmatch(SEXP target, SEXP input, size_t slen)
{
    const char *st = "";
    const void *vmax = vmaxget();

    if(target == R_NilValue)
	return NO_MATCH;

    switch (TYPEOF(target)) {
    case SYMSXP:
	st = R_CHAR(PRINTNAME(target));
	break;
    case CHARSXP:
	st = Rf_translateChar(target);
	break;
    default:  // -Wswitch
	break;
    }
    if(streqln(st, Rf_translateChar(input), slen)) {
	vmaxset(vmax);
	return (strlen(st) == slen) ?  EXACT_MATCH : PARTIAL_MATCH;
    } else {
	vmaxset(vmax);
	return NO_MATCH;
    }
}

SEXP attribute_hidden
Rf_fixSubset3Args(SEXP call, SEXP args, SEXP env, SEXP* syminp)
{
    SEXP input, nlist;
	//ArgList arglist({ CAR(args), input }, ArgList::RAW);
    /* first translate CADR of args into a string so that we can
       pass it down to DispatchorEval and have it behave correctly */

    PROTECT(input = Rf_allocVector(STRSXP, 1));
    nlist = CADR(args);
    if (TYPEOF(nlist) == PROMSXP)
	nlist = Rf_eval(nlist, env);
    if(Rf_isSymbol(nlist)) {
	if (syminp != NULL)
	    *syminp = nlist;
	SET_STRING_ELT(input, 0, PRINTNAME(nlist));
    } else if(Rf_isString(nlist) )
	SET_STRING_ELT(input, 0, STRING_ELT(nlist, 0));
    else {
	Rf_errorcall(call,_("invalid subscript type '%s'"),
		  Rf_type2char(TYPEOF(nlist)));
	return R_NilValue; /*-Wall*/
    }

    /* replace the second argument with a string */

    /* Previously this was SETCADR(args, input); */
    /* which could cause problems when nlist was */
    /* ..., as in PR#8718 */

    args = Rf_shallow_duplicate(args);
    SETCADR(args, input);
    UNPROTECT(1); /* input */
    return args;
}

/* The $ subset operator.
   We need to be sure to only evaluate the first argument.
   The second will be a symbol that needs to be matched, not evaluated.
*/
SEXP attribute_hidden do_subset3(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP ans;
	ArgList arglist(SEXP_downcast<PairList*>(Rf_fixSubset3Args(call, args, env, nullptr)), ArgList::RAW);

    auto dispatched = Rf_DispatchOrEval(SEXP_downcast<Expression*>(call),
                                        SEXP_downcast<BuiltInFunction*>(op),
                                        &arglist,
                                        SEXP_downcast<Environment*>(env),
                                        MissingArgHandling::Keep);
    if (dispatched.first) {
        ans = dispatched.second;
	if (NAMED(ans))
	    ENSURE_NAMEDMAX(ans);
	return(ans);
    }

    return R_subset3_dflt(arglist.get(0), STRING_ELT(arglist.get(1), 0), call);
}

/* used in eval.cpp */
SEXP attribute_hidden R_subset3_dflt(SEXP x, SEXP input, SEXP call)
{
    SEXP y, nlist;
    size_t slen;

    PROTECT(x);
    PROTECT(input);

    /* Optimisation to prevent repeated recalculation */
    slen = strlen(Rf_translateChar(input));
     /* The mechanism to allow a class extending "environment" */
    if( IS_S4_OBJECT(x) && TYPEOF(x) == S4SXP ){
	x = R_getS4DataSlot(x, ANYSXP);
	if(x == R_NilValue)
	    Rf_errorcall(call, "$ operator not defined for this S4 class");
    }

    /* If this is not a list object we return NULL. */

    if (Rf_isPairList(x)) {
	SEXP xmatch = R_NilValue;
	int havematch;
	UNPROTECT(2);
	havematch = 0;
	for (y = x ; y != R_NilValue ; y = CDR(y)) {
	    switch(pstrmatch(TAG(y), input, slen)) {
	    case EXACT_MATCH:
		y = CAR(y);
		if (NAMED(x) > NAMED(y)) SET_NAMED(y, NAMED(x));
		return y;
	    case PARTIAL_MATCH:
		havematch++;
		xmatch = y;
		break;
	    case NO_MATCH:
		break;
	    }
	}
	if (havematch == 1) { /* unique partial match */
	    if(R_warn_partial_match_dollar) {
		const char *st = "";
		SEXP target = TAG(xmatch);
		switch (TYPEOF(target)) {
		case SYMSXP:
		    st = R_CHAR(PRINTNAME(target));
		    break;
		case CHARSXP:
		    st = Rf_translateChar(target);
		    break;
		default:
		    throw std::logic_error("Unexpected SEXPTYPE");
		}
		Rf_warningcall(call, _("partial match of '%s' to '%s'"),
			    Rf_translateChar(input), st);
	    }
	    y = CAR(xmatch);
	    if (NAMED(x) > NAMED(y)) SET_NAMED(y, NAMED(x));
	    return y;
	}
	return R_NilValue;
    }
    else if (Rf_isVectorList(x)) {
	R_xlen_t i, n, imatch = -1;
	int havematch;
	nlist = Rf_getAttrib(x, R_NamesSymbol);
	UNPROTECT(2);
	n = Rf_xlength(nlist);
	havematch = 0;
	for (i = 0 ; i < n ; i = i + 1) {
	    switch(pstrmatch(STRING_ELT(nlist, i), input, slen)) {
	    case EXACT_MATCH:
		y = VECTOR_ELT(x, i);
		if (NAMED(x) > NAMED(y))
		    SET_NAMED(y, NAMED(x));
		return y;
	    case PARTIAL_MATCH:
		havematch++;
		if (havematch == 1) {
		    /* partial matches can cause aliasing in eval.cpp:evalseq
		       This is overkill, but alternative ways to prevent
		       the aliasing appear to be even worse */
		    y = VECTOR_ELT(x,i);
		    ENSURE_NAMEDMAX(y);
		    SET_VECTOR_ELT(x,i,y);
		}
		imatch = i;
		break;
	    case NO_MATCH:
		break;
	    }
	}
	if(havematch == 1) { /* unique partial match */
	    if(R_warn_partial_match_dollar) {
		const char *st = "";
		SEXP target = STRING_ELT(nlist, imatch);
		switch (TYPEOF(target)) {
		case SYMSXP:
		    st = R_CHAR(PRINTNAME(target));
		    break;
		case CHARSXP:
		    st = Rf_translateChar(target);
		    break;
		default:
		    throw std::logic_error("Unexpected SEXPTYPE");
		}
		Rf_warningcall(call, _("partial match of '%s' to '%s'"),
			    Rf_translateChar(input), st);
	    }
	    y = VECTOR_ELT(x, imatch);
	    if (NAMED(x) > NAMED(y)) SET_NAMED(y, NAMED(x));
	    return y;
	}
	return R_NilValue;
    }
    else if( Rf_isEnvironment(x) ){
	y = Rf_findVarInFrame(x, Rf_installTrChar(input));
	if( TYPEOF(y) == PROMSXP ) {
	    PROTECT(y);
	    y = Rf_eval(y, R_GlobalEnv);
	    UNPROTECT(1);
	}
	UNPROTECT(2);
	if( y != R_UnboundValue ) {
	    if (NAMED(y))
		ENSURE_NAMEDMAX(y);
	    else if (NAMED(x) > NAMED(y))
		SET_NAMED(y, NAMED(x));
	    return(y);
	}
	return R_NilValue;
    }
    else if( Rf_isVectorAtomic(x) ){
	Rf_errorcall(call, "$ operator is invalid for atomic vectors");
    }
    else /* e.g. a function */
	Rf_errorcall(call, _("object of type '%s' is not subsettable"), Rf_type2char(TYPEOF(x)));
    UNPROTECT(2);
    return R_NilValue;
}
