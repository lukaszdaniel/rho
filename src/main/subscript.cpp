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
 *
 * EXPORTS:
 *
 *  OneIndex()        -- used for "[[<-" in ./subassign.cpp
 *  Rf_get1index()       -- used for "[["   in ./subassign.cpp & subset.cpp
 *  vectorIndex()     -- used for "[[" and "[[<-" with a vector arg

 *  mat2indsub()      -- for "mat[i]"     "    "            "

 *  makeSubscript()   -- for "[" and "[<-" in ./subset.cpp and ./subassign.cpp,
 *			 and "[[<-" with a scalar in ./subassign.cpp
 *  arraySubscript()  -- for "[i,j,..." and "[<-..." in ./subset.cpp, ./subassign.cpp
 */

#define R_NO_REMAP

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Localization.h>

#include <rho/GCStackRoot.hpp>
#include <rho/Subscripting.hpp>

using namespace std;
using namespace rho;

/* We might get a call with nullptr from subassignment code */
#define ECALL(call, yy)     if(call == nullptr) Rf_error(yy);    else Rf_errorcall(call, yy);
#define ECALL3(call, yy, A) if(call == nullptr) Rf_error(yy, A); else Rf_errorcall(call, yy, A);

/* This allows for the unusual case where x is of length 2,
   and x[[-m]] selects one element for m = 1, 2.
   So 'len' is only used if it is 2 and i is negative.
*/
R_INLINE static int integerOneIndex(int i, R_xlen_t len, SEXP call)
{
    int indx = -1;

    if (i > 0) /* a regular 1-based index from R */
	indx = i - 1;
    else if (i == 0 || len < 2) {
	ECALL3(call, _("attempt to select less than one element in %s"), "integerOneIndex");
    } else if (len == 2 && i > -3)
	indx = 2 + i;
    else {
	ECALL3(call, _("attempt to select more than one element in %s"), "integerOneIndex");
    }
    return indx;
}

/* Utility used (only in) do_subassign2_dflt(), i.e. "[[<-" in ./subassign.cpp : */
HIDDEN R_xlen_t Rf_OneIndex(SEXP x, SEXP s, R_xlen_t nx, int partial, SEXP *newname,
	 int pos, SEXP call)
{
    SEXP names;
    R_xlen_t i, indx;
    const void *vmax;

    if (pos < 0 && Rf_length(s) > 1) {
	ECALL3(call, _("attempt to select more than one element in %s"), "OneIndex");
    }
    if (pos < 0 && Rf_length(s) < 1) {
	ECALL3(call, _("attempt to select less than one element in %s"), "OneIndex");
    }

    if(pos < 0) pos = 0;

    indx = -1;
    *newname = nullptr;
    switch(TYPEOF(s)) {
    case LGLSXP:
    case INTSXP:
	indx = integerOneIndex(INTEGER_ELT(s, pos), nx, call);
	break;
    case REALSXP:
	indx = integerOneIndex(int(REAL_ELT(s, pos)), nx, call);
	break;
    case STRSXP:
	vmax = vmaxget();
	names = Rf_getAttrib(x, Symbols::NamesSymbol);
	if (names != nullptr) {
	    PROTECT(names);
	    /* Try for exact match */
	    for (i = 0; i < nx; i++) {
		const char *tmp = Rf_translateChar(STRING_ELT(names, i));
		if (!tmp[0]) continue;
		if (streql(tmp, Rf_translateChar(STRING_ELT(s, pos)))) {
		    indx = i;
		    break;
		}
	    }
	    // Try for partial match -- not ever used in current R (partial is 0)
	    if (partial && indx < 0) {
		size_t l = strlen(Rf_translateChar(STRING_ELT(s, pos)));
		for(i = 0; i < nx; i++) {
		    const char *tmp = Rf_translateChar(STRING_ELT(names, i));
		    if (!tmp[0]) continue;
		    if(streqln(tmp, Rf_translateChar(STRING_ELT(s, pos)), l)) {
			if(indx == -1 )
			    indx = i;
			else
			    indx = -2;
		    }
		}
	    }
	    UNPROTECT(1); /* names */
	}
	if (indx == -1)
	    indx = nx;
	*newname = STRING_ELT(s, pos);
	vmaxset(vmax);
	break;
    case SYMSXP:
	vmax = vmaxget();
	names = Rf_getAttrib(x, Symbols::NamesSymbol);
	if (names != nullptr) {
	    PROTECT(names);
	    for (i = 0; i < nx; i++)
		if (streql(Rf_translateChar(STRING_ELT(names, i)),
			   Rf_translateChar(PRINTNAME(s)))) {
		    indx = i;
		    break;
		}
	    UNPROTECT(1); /* names */
	}
	if (indx == -1)
	    indx = nx;
	*newname = PRINTNAME(s);
	vmaxset(vmax);
	break;
    default:
	ECALL3(call, _("invalid subscript type '%s'"), Rf_type2char(TYPEOF(s)));
    }
    return indx;
}

/* used here and in subset.cpp and subassign.cpp */
HIDDEN R_xlen_t Rf_get1index(SEXP s, SEXP names, R_xlen_t len, int pok, int pos, SEXP call)
{
/* Get a single index for the [[ and [[<- operators.
   Checks that only one index is being selected.
   Returns -1 for no match.

   s is the subscript
   len is the length of the object or dimension, with names its (dim)names.
   pos is len-1 or -1 for [[, -1 for [[<-
     -1 means use the only element of length-1 s.
   pok : is "partial ok" ?
	 if pok is -1, warn if partial matching occurs, but allow.
*/
    int  warn_pok = 0;
    R_xlen_t indx;
    const void *vmax;

    if (pok == -1) {
	pok = 1;
	warn_pok = true;
    }

    if (pos < 0 && Rf_length(s) != 1) {
	if (Rf_length(s) > 1) {
	    ECALL3(call, _("attempt to select more than one element in %s"), "get1index");
	} else {
	    ECALL3(call, _("attempt to select less than one element in %s"), "get1index");
	}
    } else
	if (pos >= Rf_length(s)) {
	    ECALL(call, _("internal error in use of recursive indexing"));
	}
    if (pos < 0) pos = 0;
    indx = -1;
    switch (TYPEOF(s)) {
    case LGLSXP:
    case INTSXP:
	{
	    int i = INTEGER_ELT(s, pos);
	    if (i != R_NaInt)
		indx = integerOneIndex(i, len, call);
	    break;
	}
    case REALSXP:
    {
	double dblind = REAL_ELT(s, pos);
	if(!std::isnan(dblind)) {
	    /* see comment above integerOneIndex */
	    if (dblind > 0) indx = R_xlen_t(dblind - 1);
	    else if (dblind == 0 || len < 2) {
		ECALL3(call, _("attempt to select less than one element in %s"), "get1index <real>");
	    } else if (len == 2 && dblind > -3)
		indx = R_xlen_t(2 + dblind);
	    else {
		ECALL3(call, _("attempt to select more than one element in %s"), "get1index <real>");
	    }
	}
	break;
    }
    case STRSXP:
	{
	    /* NA matches nothing */
	    if (STRING_ELT(s, pos) == R_NaString)
		break;
	    /* "" matches nothing: see names.Rd */
	    if (!R_CHAR(STRING_ELT(s, pos))[0])
		break;

	    /* Try for exact match */
	    vmax = vmaxget();
	    const char *ss = Rf_translateChar(STRING_ELT(s, pos));
	    for (R_xlen_t i = 0; i < Rf_xlength(names); i++)
		if (STRING_ELT(names, i) != R_NaString) {
		    if (streql(Rf_translateChar(STRING_ELT(names, i)), ss)) {
			indx = i;
			break;
		    }
		}
	    /* Try for partial match */
	    if (pok && indx < 0) {
		size_t len = strlen(ss);
		for(R_xlen_t i = 0; i < Rf_xlength(names); i++) {
		    if (STRING_ELT(names, i) != R_NaString) {
			const char* cur_name = Rf_translateChar(STRING_ELT(names, i));
			if (streqln(cur_name, ss, len)) {
			    if (indx == -1) {/* first one */
				indx = i;
				if (warn_pok) {
				    if (call == nullptr)
					Rf_warning(_("partial match of '%s' to '%s'"),
						ss, cur_name);
				    else
					Rf_warningcall(call,
						    _("partial match of '%s' to '%s'"),
						    ss, cur_name);
				}
			    }
			    else {
				indx = -2;/* more than one partial match */
				if (warn_pok) /* already given context */
				    Rf_warningcall(nullptr,
						_("further partial match of '%s' to '%s'"),
						ss, cur_name);
				break;
			    }
			}
		    }
		}
	    }
	    vmaxset(vmax);
	}
	break;
    case SYMSXP:
	vmax = vmaxget();
	for (R_xlen_t i = 0; i < Rf_xlength(names); i++)
	    if (STRING_ELT(names, i) != R_NaString &&
		streql(Rf_translateChar(STRING_ELT(names, i)),
		       R_CHAR(PRINTNAME(s)))) {
		indx = i;
		vmaxset(vmax);
		break;
	    }
	break;
    default:
	ECALL3(call, _("invalid subscript type '%s'"), Rf_type2char(TYPEOF(s)));
    }
    return indx;
}

/* This is used for [[ and [[<- with a vector of indices of length > 1 .
   x is a list or pairlist, and it is indexed recusively from
   level start to level stop-1.  ( 0...len-1 or 0..len-2 then len-1).
   For [[<- it needs to duplicate if substructure might be shared.
 */
HIDDEN SEXP
vectorIndex(SEXP x, SEXP thesub, int start, int stop, int pok, SEXP call,
	    Rboolean dup)
{
    int i;
    R_xlen_t offset;
    SEXP cx;

    /* sanity check */
    if (dup && MAYBE_SHARED(x))
	Rf_error("should only be called in an assignment context.");

    for(i = start; i < stop; i++) {
	if(!Rf_isVectorList(x) && !Rf_isPairList(x)) {
	    if (i)
		Rf_errorcall(call, _("recursive indexing failed at level %d\n"), i+1);
	    else
		Rf_errorcall(call, _("attempt to select more than one element in %s"), "vectorIndex");
	}
	PROTECT(x);
	SEXP names = PROTECT(Rf_getAttrib(x, Symbols::NamesSymbol));
	offset = Rf_get1index(thesub, names,
			   Rf_xlength(x), pok, i, call);
	UNPROTECT(2); /* x, names */
	if(offset < 0 || offset >= Rf_xlength(x))
	    Rf_errorcall(call, _("no such index at level %d\n"), i+1);
	if(Rf_isPairList(x)) {
#ifdef LONG_VECTOR_SUPPORT
	    if (offset > R_SHORT_LEN_MAX)
		Rf_error("invalid subscript for pairlist");
#endif
	    cx = Rf_nthcdr(x, (int) offset);
	    RAISE_NAMED(CAR(x), NAMED(x));
	    x = CAR(cx);
	    if (dup && MAYBE_SHARED(x)) {
		PROTECT(cx);
		x = Rf_shallow_duplicate(x);
		SETCAR(cx, x);
		UNPROTECT(1); /* cx */
	    }
	} else {
	    cx = x;
	    x = VECTOR_ELT(x, offset);
	    RAISE_NAMED(x, NAMED(cx));
	    if (dup && MAYBE_SHARED(x)) {
		PROTECT(cx);
		x = Rf_shallow_duplicate(x);
		SET_VECTOR_ELT(cx, offset, x);
		UNPROTECT(1); /* cx */
	    }
	}
    }
    return x;
}

/* Special Matrix Subscripting: Handles the case x[i] where
   x is an n-way array and i is a matrix with n columns.
   This code returns a vector containing the subscripts
   to be extracted when x is regarded as unravelled.

   Negative indices are not allowed.

   A zero/NA anywhere in a row will cause a zero/NA in the same
   position in the result.
*/


HIDDEN SEXP Rf_mat2indsub(SEXP dims, SEXP s, SEXP call)
{
    int nrs = Rf_nrows(s);
    R_xlen_t NR = nrs;
    SEXP rvec;
    int ndim = LENGTH(dims);
    const int *pdims = INTEGER_RO(dims);

    if (Rf_ncols(s) != ndim) {
	ECALL(call, _("incorrect number of columns in matrix subscript"));
    }

#ifdef LONG_VECTOR_SUPPORT
    /* Check if it is a long vector we need to index */
    R_xlen_t len = 1;
    for (int j = 0; j < ndim; j++)  len *= pdims[j];

    if(len > R_SHORT_LEN_MAX) {
	PROTECT(rvec = Rf_allocVector(REALSXP, nrs));
	double *rv = REAL(rvec);
	for (int i = 0; i < nrs; i++) rv[i] = 1.; // 1-based.
	if (TYPEOF(s) == REALSXP) {
	    for (int i = 0; i < nrs; i++) {
		R_xlen_t tdim = 1;
		const double *ps = REAL_RO(s);
		for (int j = 0; j < ndim; j++) {
		    double k = ps[i + j * NR];
		    if(std::isnan(k)) {rv[i] = R_NaReal; break;}
		    if(k < 0) {
			ECALL(call, _("negative values are not allowed in a matrix subscript"));
		    }
		    if(k == 0.) {rv[i] = 0.; break;}
		    if (k > pdims[j]) {
			ECALL(call, _("subscript out of bounds"));
		    }
		    rv[i] += (k - 1.) * tdim;
		    tdim *= pdims[j];
		}
	    }
	} else {
	    s = Rf_coerceVector(s, INTSXP);
	    const int *ps = INTEGER_RO(s);
	    for (int i = 0; i < nrs; i++) {
		R_xlen_t tdim = 1;
		for (int j = 0; j < ndim; j++) {
		    int k = ps[i + j * NR];
		    if(k == R_NaInt) {rv[i] = R_NaReal; break;}
		    if(k < 0) {
			ECALL(call, _("negative values are not allowed in a matrix subscript"));
		    }
		    if(k == 0) {rv[i] = 0.; break;}
		    if (k > pdims[j]) {
			ECALL(call, _("subscript out of bounds"));
		    }
		    rv[i] += double ((k - 1) * tdim);
		    tdim *= pdims[j];
		}
	    }
	}
    } else
#endif
    {
	PROTECT(rvec = Rf_allocVector(INTSXP, nrs));
	int *iv = INTEGER(rvec);
	for (int i = 0; i < nrs; i++) iv[i] = 1; // 1-based.
	s = Rf_coerceVector(s, INTSXP);
	int *ps = INTEGER(s);
	for (int i = 0; i < nrs; i++) {
	    int tdim = 1;
	    for (int j = 0; j < ndim; j++) {
		int k = ps[i + j * NR];
		if(k == R_NaInt) {iv[i] = R_NaInt; break;}
		if(k < 0) {
		    ECALL(call, _("negative values are not allowed in a matrix subscript"));
		}
		if(k == 0) {iv[i] = 0; break;}
		if (k > pdims[j]) {
		    ECALL(call, _("subscript out of bounds"));
		}
		iv[i] += (k - 1) * tdim;
		tdim *= pdims[j];
	    }
	}
    }

    UNPROTECT(1);
    return rvec;
}

/*
Special Matrix Subscripting: For the case x[i] where x is an n-way
array and i is a character matrix with n columns, this code converts i
to an integer matrix by matching against the dimnames of x. NA values
in any row of i propagate to the result.  Unmatched entries result in
a subscript out of bounds error.  */

HIDDEN SEXP Rf_strmat2intmat(SEXP s, SEXP dnamelist, SEXP call)
{
    /* XXX: assumes all args are protected */
    int nr = Rf_nrows(s), i, j, v;
    R_xlen_t idx, NR = nr;
    SEXP dnames, snames, si, sicol, s_elt;
    PROTECT(snames = Rf_allocVector(STRSXP, nr));
    PROTECT(si = Rf_allocVector(INTSXP, Rf_xlength(s)));
    Rf_dimgets(si, Rf_getAttrib(s, Symbols::DimSymbol));
    int *psi = INTEGER(si);
    for (i = 0; i < Rf_length(dnamelist); i++) {
	dnames = VECTOR_ELT(dnamelist, i);
	for (j = 0; j < nr; j++)
	    SET_STRING_ELT(snames, j, STRING_ELT(s, j + (i * NR)));
	PROTECT(sicol = Rf_match(dnames, snames, 0));
	for (j = 0; j < nr; j++) {
	    v = INTEGER_ELT(sicol, j);
	    idx = j + (i * NR);
	    s_elt = STRING_ELT(s, idx);
	    if (s_elt == R_NaString) v = R_NaInt;
	    if (!R_CHAR(s_elt)[0]) v = 0; /* disallow "" match */
	    if (v == 0) Rf_errorcall(call, _("subscript out of bounds"));
	    psi[idx] = v;
	}
	UNPROTECT(1);
    }
    UNPROTECT(2);
    return si;
}

static SEXP nullSubscript(R_xlen_t n)
{
    SEXP indx;
#ifdef LONG_VECTOR_SUPPORT
    if (n > R_SHORT_LEN_MAX) {
	indx = Rf_allocVector(REALSXP, n);
	double *pindx = REAL(indx);
	for (R_xlen_t i = 0; i < n; i++)
	    pindx[i]= double(i + 1);
    } else
#endif
    {
	indx = Rf_allocVector(INTSXP, n);
	int *pindx = INTEGER(indx);
	for (int i = 0; i < n; i++)
	    pindx[i] = i + 1;
    }
    return indx;
}


static SEXP logicalSubscript(SEXP s, R_xlen_t ns, R_xlen_t nx, R_xlen_t *stretch, SEXP call)
{
    bool canstretch = (*stretch != 0);
    *stretch = 0;
    pair<VectorBase*, size_t> pr = Subscripting::canonicalize(s, nx);
    if (R_xlen_t(pr.second) > nx) {
	if (!canstretch) {
	    ECALL(call, _("subscript out of bounds"));
	} else *stretch = pr.second;
    }
    return Rf_coerceVector(pr.first, INTSXP);
}

static SEXP integerSubscript(SEXP s, R_xlen_t ns, R_xlen_t nx, R_xlen_t *stretch, SEXP call)
{
    bool canstretch = (*stretch != 0);
    *stretch = 0;
    pair<VectorBase*, size_t> pr = Subscripting::canonicalize(s, nx);
    if (R_xlen_t(pr.second) > nx) {
	if (!canstretch) {
	    ECALL(call, _("subscript out of bounds"));
	} else *stretch = pr.second;
    }
    return Rf_coerceVector(pr.first, INTSXP);
}

static SEXP realSubscript(SEXP s, R_xlen_t ns, R_xlen_t nx, R_xlen_t *stretch, SEXP call)
{
    R_xlen_t i;
    int canstretch;
    double ii, min, max;
    Rboolean isna = FALSE;
    canstretch = *stretch > 0;
    *stretch = 0;
    min = 0;
    max = 0;
    const double *ps = REAL_RO(s);
    for (i = 0; i < ns; i++) {
	ii = ps[i];
	if (std::isfinite(ii)) {
	    if (ii < min) min = ii;
	    if (ii > max) max = ii;
	} else isna = TRUE;
    }
    if (max > nx) {
#ifndef LONG_VECTOR_SUPPORT
	if (max > INT_MAX) {
	    ECALL(call, _("subscript too large for 32-bit R"));
	}
#endif
	if(canstretch) *stretch = R_xlen_t(max);
	else {
	    ECALL(call, _("subscript out of bounds"));
	}
    }
    if (min < 0) {
	if (max == 0 && !isna) {
	    SEXP indx;
	    R_xlen_t stretch = 0;
	    double dx;
	    R_xlen_t i, ix;
	    PROTECT(indx = Rf_allocVector(LGLSXP, nx));
	    int *pindx = LOGICAL(indx);
	    for (i = 0; i < nx; i++) pindx[i] = 1;
	    for (i = 0; i < ns; i++) {
		dx = ps[i];
		if (std::isfinite(dx) && dx != 0  && -dx <= nx) {
		    ix = R_xlen_t(-dx - 1);
		    pindx[ix] = 0;
		}
	    }
	    s = logicalSubscript(indx, nx, nx, &stretch, call);
	    UNPROTECT(1);
	    return s;
	} else {
	    ECALL(call, _("only 0's may be mixed with negative subscripts"));
	}
    } else {
	/* Only return a REALSXP index if we need to */
	SEXP indx;
	R_xlen_t i, cnt = 0;
	Rboolean int_ok = TRUE;
	/* NB, indices will be truncated eventually,
	   so need to do that to take '0' into account */
	for (i = 0; i < ns; i++) {
	    double ds = ps[i];
#ifdef OLDCODE_LONG_VECTOR
	    if (!std::isfinite(ds)) {
		if (ds > INT_MAX) int_ok = FALSE;
		cnt++;
	    } else if (R_xlen_t(ds) != 0) cnt++;
#else
	    if (std::isfinite(ds) && ds > INT_MAX) int_ok = FALSE;
	    if (!std::isfinite(ds) || (R_xlen_t) ds != 0) cnt++;
#endif
	}
	if (int_ok) {
	    indx = Rf_allocVector(INTSXP, cnt);
	    int *pindx = INTEGER(indx);
	    for (i = 0, cnt = 0; i < ns; i++) {
		double ds = ps[i];
		int ia;
		if (!std::isfinite(ds)) ia = R_NaInt;
		else ia = int(ds);
		if (ia != 0) pindx[cnt++] = ia;
	    }
	} else {
	    indx = Rf_allocVector(REALSXP, cnt);
	    double *pindx = REAL(indx);
	    for (i = 0, cnt = 0; i < ns; i++) {
		double ds = ps[i];
		if (!std::isfinite(ds) || (R_xlen_t) ds != 0) pindx[cnt++] = ds;
	    }
	}
	return indx;
    }
    return nullptr;
}

/* This uses a couple of horrible hacks in conjunction with
 * VectorAssign (in subassign.cpp).  If subscripting is used for
 * assignment, it is possible to extend a vector by supplying new
 * names, and we want to give the extended vector those names, so they
 * are returned as the use.names attribute. Also, unset elements of the vector
 * of new names (places where a match was found) are indicated by
 * setting the element of the newnames vector to NULL.
*/

static SEXP stringSubscript(SEXP sarg, R_xlen_t ns, R_xlen_t nx, SEXP namesarg,
		R_xlen_t *stretch, SEXP call)
{
    bool canstretch = (*stretch != 0);
    *stretch = 0;
    pair<VectorBase*, size_t> pr
	= Subscripting::canonicalize(sarg, nx,
				     SEXP_downcast<StringVector*>(namesarg));
    if (R_xlen_t(pr.second) > nx) {
	if (!canstretch) {
	    ECALL(call, _("subscript out of bounds"));
	} else *stretch = pr.second;
    }
    return Rf_coerceVector(pr.first, INTSXP);
}

/* Array Subscripts.
    dim is the dimension (0 to k-1)
    s is the subscript list,
    dims is the dimensions of x
    dng is a function (usually getAttrib) that obtains the dimnames
    x is the array to be subscripted.
*/

HIDDEN SEXP int_arraySubscript(int dim, SEXP s, SEXP dims, SEXP x, SEXP call)
{
    R_xlen_t stretch = 0;
    int ns = Rf_length(s);
    int nd = INTEGER_ELT(dims, dim);

    switch (TYPEOF(s)) {
    case NILSXP:
	return Rf_allocVector(INTSXP, 0);
    case LGLSXP:
	return logicalSubscript(s, ns, nd, &stretch, nullptr);
    case INTSXP:
	return integerSubscript(s, ns, nd, &stretch, nullptr);
    case REALSXP:
	{
	    GCStackRoot<> tmp(Rf_coerceVector(s, INTSXP));
	    tmp = integerSubscript(tmp, ns, nd, &stretch, nullptr);
	    return tmp;
	}
    case STRSXP:
	{
	    SEXP dnames = Rf_getAttrib(x, Symbols::DimNamesSymbol);
	    if (dnames == nullptr) {
		ECALL(0, _("no 'dimnames' attribute for array"));
	    }
	    dnames = VECTOR_ELT(dnames, dim);
	    return stringSubscript(s, ns, nd, dnames, &stretch, call);
	}
    case SYMSXP:
	if (s == R_MissingArg)
	    return nullSubscript(nd);
    default:
	ECALL3(call, _("invalid subscript type '%s'"), Rf_type2char(TYPEOF(s)));
    }
    return nullptr;
}

/* This is used by packages arules, cba, proxy and seriation. */
typedef SEXP AttrGetter(SEXP x, SEXP data);
typedef SEXP (*StringEltGetter)(SEXP x, int i);

SEXP Rf_arraySubscript(int dim, SEXP s, SEXP dims, AttrGetter dng,
	       StringEltGetter strg, SEXP x)
{
    return int_arraySubscript(dim, s, dims, x, nullptr);
}

/* Subscript creation.  The first thing we do is check to see */
/* if there are any user supplied NULL's, these result in */
/* returning a vector of length 0. */
/* if stretch is zero on entry then the vector x cannot be
   "stretched",
   otherwise, stretch returns the new required length for x
*/

HIDDEN SEXP Rf_makeSubscript(SEXP x, SEXP s, R_xlen_t *stretch, SEXP call)
{
    if (! (Rf_isVector(x) || Rf_isList(x) || Rf_isLanguage(x))) {
	ECALL(call, _("subscripting on non-vector"));
    }

    R_xlen_t nx = Rf_xlength(x);

    /* special case for simple indices -- does not duplicate */
    if (IS_SCALAR(s, INTSXP)) {
	int i = SCALAR_IVAL(s);
	if (0 < i && i <= nx) {
	    *stretch = 0;
	    return s;
	}
    }
    else if (IS_SCALAR(s, REALSXP)) {
	double di = SCALAR_DVAL(s);
	if (1 <= di && di <= nx) {
	    *stretch = 0;
	    /* We could only return a REALSXP if the value is too
	       large for an INTSXP, but, as the calling code can
	       handle REALSXP indices, returning the REALSXP
	       avoids an allocation. */
	    return s;
	}
    }

    R_xlen_t ns = Rf_xlength(s);
    SEXP ans = nullptr;
    switch (TYPEOF(s)) {
    case NILSXP:
	*stretch = 0;
	ans = Rf_allocVector(INTSXP, 0);
	break;
    case LGLSXP:
	ans = logicalSubscript(s, ns, nx, stretch, call);
	break;
    case INTSXP:
	ans = integerSubscript(s, ns, nx, stretch, call);
	break;
    case REALSXP:
	ans = realSubscript(s, ns, nx, stretch, call);
	break;
    case STRSXP:
    {
	SEXP names = Rf_getAttrib(x, Symbols::NamesSymbol);
	/* *stretch = 0; */
	ans = stringSubscript(s, ns, nx, names, stretch, call);
	break;
    }
    case SYMSXP:
	*stretch = 0;
	if (s == R_MissingArg) {
	    ans = nullSubscript(nx);
	    break;
	}
    default:
	ECALL3(call, _("invalid subscript type '%s'"), Rf_type2char(TYPEOF(s)));
    }
    return ans;
}
