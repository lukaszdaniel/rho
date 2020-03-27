/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-2017   The R Core Team
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

/*		Warnings/Errors

    In this file we generally do not make use of the call, as it
    will be something like `[<-`(`*tmp`, ...) and that just confuses
    the user.  The call that is deduced from the context is generally
    much clearer.
 */

/*
 *
 *  Subset Mutation for Lists and Vectors
 *
 *  The following table shows the codes which have been assigned to the
 *  type combinations in assignments of the form "x[s] <- y".  Here the
 *  type of y is given across the top of the table and the type of x at
 *  the side.  (Note: the lack of 11 and 12 indices here is due to the
 *  removal of built-in factors).
 *
 *  NB these tables are out of date, and exclude types 21, 22, 23, 24 ...
 *
 x \ y   NIL  SYM CLOS  ENV PROM LANG SPE- BUI-  LGL  INT REAL CPLX  STR  VEC EXPR  FUN
				      CIAL LTIN
 LANG    600  601  603  604  605  606  607  608  610  613  614  615  616  619  620  699
 LGL    1000 1001 1003 1004 1005 1006 1007 1008 1010 1013 1014 1015 1016 1019 1020 1099
 INT    1300 1301 1303 1304 1305 1306 1307 1308 1310 1313 1314 1315 1316 1319 1320 1399
 REAL   1400 1401 1403 1404 1405 1406 1407 1408 1410 1413 1414 1415 1416 1419 1420 1499
 CPLX   1500 1501 1503 1504 1505 1506 1507 1508 1510 1513 1514 1515 1516 1519 1520 1599
 STR    1600 1601 1603 1604 1605 1606 1607 1608 1610 1613 1614 1615 1616 1619 1620 1699
 VEC    1900 1901 1903 1904 1905 1906 1907 1908 1910 1913 1914 1915 1916 1919 1920 1999
 EXPR   2000 2001 2003 2004 2005 2006 2007 2008 2010 2013 2014 2015 2016 2019 2020 2099
 *
 *
 *  The following table (which is laid out as described above) contains
 *  "*" for those combinations where the assignment has been implemented.
 *  Some assignments do not make a great deal of sense and we have chosen
 *  to leave them unimplemented, although the addition of new assignment
 *  combinations represents no great difficulty.
 *
 *       NIL   SYM CLOS  ENV PROM LANG SPE- BUI-  LGL  INT REAL CPLX  STR  VEC EXPR  FUN
 *				       CIAL LTIN
 LANG
 LGL						   *    *    *    *    *    *    *
 INT						   *    *    *    *    *    *    *
 REAL						   *    *    *    *    *    *    *
 CPLX						   *    *    *    *    *    *    *
 STR						   *    *    *    *    *    *    *
 VEC      *     *    *    *    *    *    *    *    *    *    *    *    *    *    *    *
 EXPR     *     *                   *		   *    *    *    *    *    *    *
 *
 *  The reason for the LGL row and column are because we want to allow any
 *  assignment of the form "x[s] <- NA" (col) and because the interpreted
 *  "ifelse" requires assignment into a logical object.
 */

/*
 *  2000/02/17  Altered to allow closures/primitives in lists (VECSXPs) BDR
 */

/*
 *  2000/08/01  Also promises, expressions, environments when using [[ PD
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Localization.h>
#include <Internal.h>
#include <R_ext/RS.h> /* for test of S4 objects */
#include "rho/ComplexVector.hpp"
#include "rho/ExpressionVector.hpp"
#include "rho/GCStackRoot.hpp"
#include "rho/Promise.hpp"
#include "rho/RawVector.hpp"
#include "rho/Subscripting.hpp"

using namespace rho;

static R_INLINE SEXP getNames(SEXP x)
{
    /* defer to getAttrib if a 'dim' attribute is present */
    for (SEXP attr = ATTRIB(x); attr != R_NilValue; attr = CDR(attr))
	if (TAG(attr) == R_DimSymbol)
	    return Rf_getAttrib(x, R_NamesSymbol);

    /* don't use getAttrib since that would mark as immutable */
    for (SEXP attr = ATTRIB(x); attr != R_NilValue; attr = CDR(attr))
	if (TAG(attr) == R_NamesSymbol)
	    return CAR(attr);

    return R_NilValue;
}

/* EnlargeVector() takes a vector "x" and changes its length to "newlen".
   This allows to assign values "past the end" of the vector or list.
   Overcommit by a small percentage to allow more efficient vector growth.
*/
static SEXP EnlargeNames(SEXP, R_xlen_t, R_xlen_t);

static SEXP EnlargeVector(SEXP x, R_xlen_t newlen)
{
    R_xlen_t len, newtruelen;
    SEXP newx, names, newnames;
    static SEXP R_CheckBoundsSymbol = NULL;

    if (R_CheckBoundsSymbol == NULL)
	R_CheckBoundsSymbol = Rf_install("check.bounds");

    /* Sanity Checks */
    if (!Rf_isVector(x))
	Rf_error(_("attempt to enlarge non-vector"));

    /* Enlarge the vector itself. */
    len = Rf_xlength(x);
    if (LOGICAL(Rf_GetOption1(Rf_install("check.bounds")))[0])
	Rf_warning(_("assignment outside vector/list limits (extending from %d to %d)"),
		len, newlen);

    /* if the vector is not shared, is growable. and has room, then
       increase its length */
    if (RHO_FALSE && ! MAYBE_SHARED(x) &&
	//IS_GROWABLE(x) &&
	TRUELENGTH(x) >= newlen) {
	SETLENGTH(x, newlen);
	names = getNames(x);
	if (!Rf_isNull(names)) {
	    newnames = EnlargeNames(names, len, newlen);
	    if (names != newnames)
		Rf_setAttrib(x, R_NamesSymbol, newnames);
	}
	return x;
    }

    /* over-committing by 5% seems to be reasonable, but for
       experimenting the environment variable R_EXPAND_Frac can be
       used to adjust this */
    static double expand_dflt = 1.05;
    static double expand = 0;
    if (expand == 0) {
	char *envval = getenv("R_EXPAND_FRAC");
	expand = envval != NULL ? atof(envval) : expand_dflt;
	if (expand < 1 || expand > 2) {
	    expand = expand_dflt;
	    Rf_error("bad expand value");
	}
    }

    if (newlen > len)
	newtruelen = (R_xlen_t) (newlen * expand);
    else
	/* sometimes this is called when no expansion is needed */
	newtruelen = newlen;

    /**** for now, don't cross the long vector boundary; drop when
	  ALTREP is merged */
#ifdef ALTREP
#error drop the limitation to short vectors
#endif
    if (newtruelen > R_LEN_T_MAX) newtruelen = newlen;

    PROTECT(x);
    PROTECT(newx = Rf_allocVector(TYPEOF(x), newtruelen));

    /* Copy the elements into place. */
    switch(TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
	for (R_xlen_t i = 0; i < len; i++)
	    INTEGER(newx)[i] = INTEGER(x)[i];
	for (R_xlen_t i = len; i < newlen; i++)
	    INTEGER(newx)[i] = NA_INTEGER;
	break;
    case REALSXP:
	for (R_xlen_t i = 0; i < len; i++)
	    REAL(newx)[i] = REAL(x)[i];
	for (R_xlen_t i = len; i < newlen; i++)
	    REAL(newx)[i] = NA_REAL;
	break;
    case CPLXSXP:
	for (R_xlen_t i = 0; i < len; i++)
	    COMPLEX(newx)[i] = COMPLEX(x)[i];
	for (R_xlen_t i = len; i < newtruelen; i++) {
	    COMPLEX(newx)[i].r = NA_REAL;
	    COMPLEX(newx)[i].i = NA_REAL;
	}
	break;
    case STRSXP:
	for (R_xlen_t i = 0; i < len; i++)
	    SET_STRING_ELT(newx, i, STRING_ELT(x, i));
	for (R_xlen_t i = len; i < newtruelen; i++)
	    SET_STRING_ELT(newx, i, NA_STRING); /* was R_BlankString  < 1.6.0 */
	break;
    case EXPRSXP:
	for (R_xlen_t i = 0; i < len; i++)
	    SET_XVECTOR_ELT(newx, i, XVECTOR_ELT(x, i));
	for (R_xlen_t i = len; i < newtruelen; i++)
	    SET_XVECTOR_ELT(newx, i, R_NilValue);
	break;
    case VECSXP:
	for (R_xlen_t i = 0; i < len; i++)
	    SET_VECTOR_ELT(newx, i, VECTOR_ELT(x, i));
	for (R_xlen_t i = len; i < newtruelen; i++)
	    SET_VECTOR_ELT(newx, i, R_NilValue);
	break;
    case RAWSXP:
	for (R_xlen_t i = 0; i < len; i++)
	    RAW(newx)[i] = RAW(x)[i];
	for (R_xlen_t i = len; i < newtruelen; i++)
	    RAW(newx)[i] = Rbyte( 0);
	break;
    default:
	UNIMPLEMENTED_TYPE("EnlargeVector", x);
    }
    if (newlen < newtruelen) {
	//SET_GROWABLE_BIT(newx);
	SET_TRUELENGTH(newx, newtruelen);
	SETLENGTH(newx, newlen);
    }

    /* Adjust the attribute list. */
    names = getNames(x);
    if (!Rf_isNull(names))
	Rf_setAttrib(newx, R_NamesSymbol, EnlargeNames(names, len, newlen));
    Rf_copyMostAttrib(x, newx);
    UNPROTECT(2);
    return newx;
}

static SEXP EnlargeNames(SEXP names, R_xlen_t len, R_xlen_t newlen)
{
    if (TYPEOF(names) != STRSXP || XLENGTH(names) != len)
	Rf_error(_("bad names attribute"));
    SEXP newnames = PROTECT(EnlargeVector(names, newlen));
    for (R_xlen_t i = len; i < newlen; i++)
	SET_STRING_ELT(newnames, i, R_BlankString);
    UNPROTECT(1);
    return newnames;
}

/* used instead of coerceVector to embed a non-vector in a list for
   purposes of SubassignTypeFix, for cases in wich coerceVector should
   fail; namely, S4SXP */
static SEXP embedInVector(SEXP v, SEXP call)
{
    SEXP ans;
    Rf_warningcall(call, "implicit list embedding of S4 objects is deprecated");
    PROTECT(ans = Rf_allocVector(VECSXP, 1));
    SET_VECTOR_ELT(ans, 0, v);
    UNPROTECT(1);
    return (ans);
}

static std::pair<bool, SEXP> dispatch_asvector(SEXP x, const Expression* call,
                                               Environment* rho)
{
    static GCRoot<BuiltInFunction> op
        = BuiltInFunction::obtainPrimitive("as.vector");
    static GCRoot<> any = Rf_mkString("any");
    ArgList args({ x, any }, ArgList::EVALUATED);
    return Rf_Dispatch(call, op, args, rho);
}


/* Level 1 is used in VectorAssign, ArrayAssign.
   That coerces RHS to a list or expression.

   Level 2 is used in do_subassign2_dflt.
   This does not coerce when assigning into a list.
*/

static int SubassignTypeFix(SEXP *x, SEXP *y, int level, SEXP call, SEXP rho)
{
    int which = 100 * TYPEOF(*x) + TYPEOF(*y);

    switch (which) {
    case 1000:	/* logical    <- null       */
    case 1300:	/* integer    <- null       */
    case 1400:	/* real	      <- null       */
    case 1500:	/* complex    <- null       */
    case 1600:	/* character  <- null       */
    case 1900:  /* vector     <- null       */
    case 2000:  /* expression <- null       */
    case 2400:	/* raw        <- null       */

    case 1010:	/* logical    <- logical    */
    case 1310:	/* integer    <- logical    */
    case 1410:	/* real	      <- logical    */
    case 1510:	/* complex    <- logical    */
    case 1313:	/* integer    <- integer    */
    case 1413:	/* real	      <- integer    */
    case 1513:	/* complex    <- integer    */
    case 1414:	/* real	      <- real	    */
    case 1514:	/* complex    <- real	    */
    case 1515:	/* complex    <- complex    */
    case 1616:	/* character  <- character  */
    case 1919:  /* vector     <- vector     */
    case 2020:	/* expression <- expression */
    case 2424:	/* raw        <- raw        */

	break;

    case 1013:	/* logical    <- integer    */

	*x = Rf_coerceVector(*x, INTSXP);
	break;

    case 1014:	/* logical    <- real	    */
    case 1314:	/* integer    <- real	    */

	*x = Rf_coerceVector(*x, REALSXP);
	break;

    case 1015:	/* logical    <- complex    */
    case 1315:	/* integer    <- complex    */
    case 1415:	/* real	      <- complex    */

	*x = Rf_coerceVector(*x, CPLXSXP);
	break;

    case 1610:	/* character  <- logical    */
    case 1613:	/* character  <- integer    */
    case 1614:	/* character  <- real	    */
    case 1615:	/* character  <- complex    */

	*y = Rf_coerceVector(*y, STRSXP);
	break;

    case 1016:	/* logical    <- character  */
    case 1316:	/* integer    <- character  */
    case 1416:	/* real	      <- character  */
    case 1516:	/* complex    <- character  */

	*x = Rf_coerceVector(*x, STRSXP);
	break;

    case 1901:  /* vector     <- symbol   */
    case 1902:	/* vector     <- pairlist */
    case 1904:  /* vector     <- environment   */
    case 1905:  /* vector     <- promise   */
    case 1906:  /* vector     <- language   */
    case 1910:  /* vector     <- logical    */
    case 1913:  /* vector     <- integer    */
    case 1914:  /* vector     <- real       */
    case 1915:  /* vector     <- complex    */
    case 1916:  /* vector     <- character  */
    case 1920:  /* vector     <- expression  */
    case 1921:  /* vector     <- bytecode   */
    case 1922:  /* vector     <- external pointer */
    case 1923:  /* vector     <- weak reference */
    case 1924:  /* vector     <- raw */
    case 1903: case 1907: case 1908: case 1999: /* functions */

	if (level == 1) {
	    /* Coerce the RHS into a list */
	    *y = Rf_coerceVector(*y, VECSXP);
	} else {
	    /* Nothing to do here: duplicate when used (if needed) */
	}
	break;

    case 1925: /* vector <- S4 */

	if (level == 1) {
	    /* Embed the RHS into a list */
	    *y = embedInVector(*y, call);
	} else {
	    /* Nothing to do here: duplicate when used (if needed) */
	}
	break;

    case 1019:  /* logical    <- vector     */
    case 1319:  /* integer    <- vector     */
    case 1419:  /* real       <- vector     */
    case 1519:  /* complex    <- vector     */
    case 1619:  /* character  <- vector     */
    case 2419:  /* raw        <- vector     */
	*x = Rf_coerceVector(*x, VECSXP);
	break;

    case 1020:  /* logical    <- expression */
    case 1320:  /* integer    <- expression */
    case 1420:  /* real       <- expression */
    case 1520:  /* complex    <- expression */
    case 1620:  /* character  <- expression */
    case 2420:  /* raw        <- expression */
	*x = Rf_coerceVector(*x, EXPRSXP);
	break;

    case 2001:	/* expression <- symbol	    */
    case 2002:  /* expression <- pairlist   */
    case 2006:	/* expression <- language   */
    case 2010:	/* expression <- logical    */
    case 2013:	/* expression <- integer    */
    case 2014:	/* expression <- real	    */
    case 2015:	/* expression <- complex    */
    case 2016:	/* expression <- character  */
    case 2019:  /* expression <- vector     */

	if (level == 1) {
	    /* Coerce the RHS into a list */
	    *y = Rf_coerceVector(*y, VECSXP);
	} else {
	    /* Note : No coercion is needed here. */
	    /* We just insert the RHS into the LHS. */
	}
	break;

    case 2025: /* expression <- S4 */

	if (level == 1) {
	    /* Embed the RHS into a list */
	    *y = embedInVector(*y, call);
	} else {
	    /* Nothing to do here: duplicate when used (if needed) */
	}
	break;

    case 1025: /* logical   <- S4 */
    case 1325: /* integer   <- S4 */
    case 1425: /* real      <- S4 */
    case 1525: /* complex   <- S4 */
    case 1625: /* character <- S4 */
    case 2425: /* raw       <- S4 */
      {
          auto dispatched = dispatch_asvector(*y,
                                              SEXP_downcast<Expression*>(call),
                                              SEXP_downcast<Environment*>(rho));
          if (dispatched.first) {
              *y = dispatched.second;
              return SubassignTypeFix(x, y, level, call, rho);
          }
      }
    default:
	Rf_error(_("incompatible types (from %s to %s) in subassignment type fix"),
	      Rf_type2char(RHOCONSTRUCT(SEXPTYPE, which%100)), Rf_type2char(RHOCONSTRUCT(SEXPTYPE, which/100)));
    }

    return(100 * TYPEOF(*x) + TYPEOF(*y));
}

#ifdef LONG_VECTOR_SUPPORT
static R_INLINE R_xlen_t gi(SEXP indx, R_xlen_t i)
{
    if (TYPEOF(indx) == REALSXP) {
	double d = REAL(indx)[i];
	return R_FINITE(d) ? R_xlen_t( d) : NA_INTEGER;
    } else
	return INTEGER(indx)[i];
}
#else
#define R_SHORT_LEN_MAX INT_MAX
static R_INLINE int gi(SEXP indx, R_xlen_t i)
{
    if (TYPEOF(indx) == REALSXP) {
	double d = REAL(indx)[i];
	if (!R_FINITE(d) || d < -R_SHORT_LEN_MAX || d > R_SHORT_LEN_MAX) return NA_INTEGER;
	return int( d);
    } else
	return INTEGER(indx)[i];
}
#endif

static SEXP DeleteListElements(SEXP x, SEXP which)
{
    SEXP include, xnew, xnames, xnewnames;
    R_xlen_t i, ii, len, lenw;
    len = Rf_xlength(x);
    lenw = Rf_xlength(which);
    /* calculate the length of the result */
    PROTECT(include = Rf_allocVector(INTSXP, len));
    for (i = 0; i < len; i++)
	INTEGER(include)[i] = 1;
    for (i = 0; i < lenw; i++) {
	ii = gi(which, i);
	if (0 < ii  && ii <= len)
	    INTEGER(include)[ii - 1] = 0;
    }
    ii = 0;
    for (i = 0; i < len; i++)
	ii += INTEGER(include)[i];
    if (ii == len) {
	UNPROTECT(1);
	return x;
    }
    PROTECT(xnew = Rf_allocVector(TYPEOF(x), ii));
    ii = 0;
    switch (TYPEOF(x)) {
    case VECSXP:
	for (i = 0; i < len; i++) {
	    if (INTEGER(include)[i] == 1) {
		SET_VECTOR_ELT(xnew, ii, VECTOR_ELT(x, i));
		ii++;
	    }
	}
	break;
    case EXPRSXP:
	for (i = 0; i < len; i++) {
	    if (INTEGER(include)[i] == 1) {
		SET_XVECTOR_ELT(xnew, ii, XVECTOR_ELT(x, i));
		ii++;
	    }
	}
	break;
    default:
	Rf_error(_("Internal error: unexpected type in DeleteListElements"));
    }
    xnames = Rf_getAttrib(x, R_NamesSymbol);
    if (xnames != R_NilValue) {
	PROTECT(xnewnames = Rf_allocVector(STRSXP, ii));
	ii = 0;
	for (i = 0; i < len; i++) {
	    if (INTEGER(include)[i] == 1) {
		SET_STRING_ELT(xnewnames, ii, STRING_ELT(xnames, i));
		ii++;
	    }
	}
	Rf_setAttrib(xnew, R_NamesSymbol, xnewnames);
	UNPROTECT(1);
    }
    Rf_copyMostAttrib(x, xnew);
    UNPROTECT(2);
    return xnew;
}

static SEXP VectorAssign(SEXP call, SEXP rho, SEXP xarg, SEXP sarg, SEXP yarg)
{
    GCStackRoot<> x(xarg);
    GCStackRoot<> s(sarg);
    GCStackRoot<> y(yarg);

    if (Rf_isNull(x) && Rf_isNull(y)) {
	return R_NilValue;
    }

    /* Check to see if we have special matrix subscripting. */
    /* If so, we manufacture a real subscript vector. */

    PROTECT(s);
    if (ATTRIB(s) != R_NilValue) { /* pretest to speed up simple case */
	SEXP dim = Rf_getAttrib(x, R_DimSymbol);
	if (Rf_isMatrix(s) && Rf_isArray(x) && Rf_ncols(s) == Rf_length(dim)) {
	    if (Rf_isString(s)) {
		s = Rf_strmat2intmat(s, Rf_GetArrayDimnames(x), call);
		UNPROTECT(1);
		PROTECT(s);
	    }
	    if (Rf_isInteger(s) || Rf_isReal(s)) {
		s = Rf_mat2indsub(dim, s, R_NilValue);
		UNPROTECT(1);
		PROTECT(s);
	    }
	}
    }

    /* Here we make sure that the LHS has */
    /* been coerced into a form which can */
    /* accept elements from the RHS. */
    int which; /* = 100 * TYPEOF(x) + TYPEOF(y);*/
    {
	SEXP xtmp = x;
	SEXP ytmp = y;
	which = SubassignTypeFix(&xtmp, &ytmp, 1, call, rho);
	x = xtmp;
	y = ytmp;
    }

    /* Note that we are now committed. */
    /* Since we are mutating existing objects, */
    /* any changes we make now are (likely to be) permanent.  Beware! */
    switch(which) {
	/* because we have called SubassignTypeFix the commented
	   values cannot occur (and would be unsafe) */

    case 1010:	/* logical   <- logical	  */
	return Subscripting::vectorSubassign(static_cast<LogicalVector*>(x.get()), s,
					     static_cast<const LogicalVector*>(y.get()));
    /* case 1013:  logical   <- integer	  */
    /* case 1014:  logical   <- real	  */
    /* case 1015:  logical   <- complex	  */
    /* case 1016:  logical   <- character */
    /* case 1019:  logical   <- vector   */
    case 1310:	/* integer   <- logical	  */
	return Subscripting::vectorSubassign(static_cast<IntVector*>(x.get()), s,
					     static_cast<const LogicalVector*>(y.get()));
    case 1313:	/* integer   <- integer	  */
	return Subscripting::vectorSubassign(static_cast<IntVector*>(x.get()), s,
					     static_cast<const IntVector*>(y.get()));
    /* case 1314:  integer   <- real	  */
    /* case 1315:  integer   <- complex	  */
    /* case 1316:  integer   <- character */
    /* case 1319:  integer    <- vector   */
    case 1410:	/* real	     <- logical	  */
	return Subscripting::vectorSubassign(static_cast<RealVector*>(x.get()), s,
					     static_cast<const LogicalVector*>(y.get()));
    case 1413:	/* real	     <- integer	  */
	return Subscripting::vectorSubassign(static_cast<RealVector*>(x.get()), s,
					     static_cast<const IntVector*>(y.get()));
    case 1414:	/* real	     <- real	  */
	return Subscripting::vectorSubassign(static_cast<RealVector*>(x.get()), s,
					     static_cast<const RealVector*>(y.get()));
    /* case 1415:  real	     <- complex	  */
    /* case 1416:  real	     <- character */
    /* case 1419:  real       <- vector   */
    case 1510:	/* complex   <- logical	  */
	return Subscripting::vectorSubassign(static_cast<ComplexVector*>(x.get()), s,
					     static_cast<const LogicalVector*>(y.get()));
    case 1513:	/* complex   <- integer	  */
	return Subscripting::vectorSubassign(static_cast<ComplexVector*>(x.get()), s,
					     static_cast<const IntVector*>(y.get()));
    case 1514:	/* complex   <- real	  */
	return Subscripting::vectorSubassign(static_cast<ComplexVector*>(x.get()), s,
					     static_cast<const RealVector*>(y.get()));
    case 1515:	/* complex   <- complex	  */
	return Subscripting::vectorSubassign(static_cast<ComplexVector*>(x.get()), s,
					     static_cast<const ComplexVector*>(y.get()));
    /* case 1516:  complex   <- character */
    /* case 1519:  complex    <- vector   */
    /* case 1610:  character <- logical	  */
    /* case 1613:  character <- integer	  */
    /* case 1614:  character <- real	  */
    /* case 1615:  character <- complex	  */
    case 1616:	/* character <- character */
	return Subscripting::vectorSubassign(static_cast<StringVector*>(x.get()), s,
					     static_cast<const StringVector*>(y.get()));
    /* case 1619:  character  <- vector   */
    /* case 1910:  vector     <- logical    */
    /* case 1913:  vector     <- integer    */
    /* case 1914:  vector     <- real       */
    /* case 1915:  vector     <- complex    */
    /* case 1916:  vector     <- character  */
    case 1919:  /* vector     <- vector     */
	return Subscripting::vectorSubassign(static_cast<ListVector*>(x.get()), s,
					     static_cast<const ListVector*>(y.get()));
    /* case 2001:  expression <- symbol	    */
    /* case 2006:  expression <- language   */
    /* case 2010:  expression <- logical    */
    /* case 2013:  expression <- integer    */
    /* case 2014:  expression <- real	    */
    /* case 2015:  expression <- complex    */
    /* case 2016:  expression <- character  */
    case 2019:	/* expression <- vector, needed if we have promoted a
		   RHS  to a list */
	return Subscripting::vectorSubassign(static_cast<ExpressionVector*>(x.get()), s,
					     static_cast<const ListVector*>(y.get()));
    case 2020:	/* expression <- expression */
	return Subscripting::vectorSubassign(static_cast<ExpressionVector*>(x.get()), s,
					     static_cast<const ExpressionVector*>(y.get()));
    case 1900:  /* vector     <- null       */
    case 2000:  /* expression <- null       */
	{
	    R_xlen_t stretch = 1;
	    GCStackRoot<> indx(Rf_makeSubscript(x, s, &stretch, R_NilValue));
	    return DeleteListElements(x, indx);
	}
    case 2424:	/* raw   <- raw	  */
	return Subscripting::vectorSubassign(static_cast<RawVector*>(x.get()), s,
					     static_cast<const RawVector*>(y.get()));
    default:
	if (Rf_xlength(y) == 0) {
	    if (y != R_NilValue
		|| (TYPEOF(x) != VECSXP && TYPEOF(x) != EXPRSXP)) {
		std::size_t num_indices
		    = Subscripting::canonicalize(s, Rf_xlength(x)).second;
		if (num_indices > 0) {
		    Rf_error(_("replacement has length zero"));
		}
	    }
	}

	Rf_warningcall(call, "sub assignment (*[*] <- *) not done; __bug?__");
    }
    return nullptr;  // -Wall
}


static SEXP ArrayAssign(SEXP call, SEXP rho, SEXP xarg, PairList* subscripts,
			SEXP yarg)
{
    GCStackRoot<> x(xarg);
    GCStackRoot<> y(yarg);

    /* Here we make sure that the LHS has been coerced into */
    /* a form which can accept elements from the RHS. */
    int which;  /* = 100 * TYPEOF(x) + TYPEOF(y);*/
    {
	SEXP xtmp = x;
	SEXP ytmp = y;
	which = SubassignTypeFix(&xtmp, &ytmp, 1, call, rho);
	x = xtmp;
	y = ytmp;
    }

    switch (which) {
    case 1010:	/* logical   <- logical	  */
	return Subscripting::arraySubassign(static_cast<LogicalVector*>(x.get()),
					    subscripts,
					    static_cast<LogicalVector*>(y.get()));
    /* case 1013:  logical   <- integer	  */
    /* case 1014:  logical   <- real	  */
    /* case 1015:  logical   <- complex	  */
    /* case 1016:  logical   <- character */
    case 1310:	/* integer   <- logical	  */
	return Subscripting::arraySubassign(static_cast<IntVector*>(x.get()),
					    subscripts,
					    static_cast<LogicalVector*>(y.get()));
    case 1313:	/* integer   <- integer	  */
	return Subscripting::arraySubassign(static_cast<IntVector*>(x.get()),
					    subscripts,
					    static_cast<IntVector*>(y.get()));
    /* case 1314:  integer   <- real	  */
    /* case 1315:  integer   <- complex	  */
    /* case 1316:  integer   <- character */
    case 1410:	/* real	     <- logical	  */
	return Subscripting::arraySubassign(static_cast<RealVector*>(x.get()),
					    subscripts,
					    static_cast<LogicalVector*>(y.get()));
    case 1413:	/* real	     <- integer	  */
	return Subscripting::arraySubassign(static_cast<RealVector*>(x.get()),
					    subscripts,
					    static_cast<IntVector*>(y.get()));
    case 1414:	/* real	     <- real	  */
	return Subscripting::arraySubassign(static_cast<RealVector*>(x.get()),
					    subscripts,
					    static_cast<RealVector*>(y.get()));
    /* case 1415:  real	     <- complex	  */
    /* case 1416:  real	     <- character */
    case 1510:	/* complex   <- logical	  */
	return Subscripting::arraySubassign(static_cast<ComplexVector*>(x.get()),
					    subscripts,
					    static_cast<LogicalVector*>(y.get()));
    case 1513:	/* complex   <- integer	  */
	return Subscripting::arraySubassign(static_cast<ComplexVector*>(x.get()),
					    subscripts,
					    static_cast<IntVector*>(y.get()));
    case 1514:	/* complex   <- real	  */
	return Subscripting::arraySubassign(static_cast<ComplexVector*>(x.get()),
					    subscripts,
					    static_cast<RealVector*>(y.get()));
    case 1515:	/* complex   <- complex	  */
	return Subscripting::arraySubassign(static_cast<ComplexVector*>(x.get()),
					    subscripts,
					    static_cast<ComplexVector*>(y.get()));
    /* case 1516:  complex   <- character */
    /* case 1610:  character <- logical	  */
    /* case 1613:  character <- integer	  */
    /* case 1614:  character <- real	  */
    /* case 1615:  character <- complex	  */
    case 1616:	/* character <- character */
	return Subscripting::arraySubassign(static_cast<StringVector*>(x.get()),
					    subscripts,
					    static_cast<StringVector*>(y.get()));
    case 1919: /* vector <- vector */
	return Subscripting::arraySubassign(static_cast<ListVector*>(x.get()),
					    subscripts,
					    static_cast<ListVector*>(y.get()));
    case 2424: /* raw <- raw */
	return Subscripting::arraySubassign(static_cast<RawVector*>(x.get()),
					    subscripts,
					    static_cast<RawVector*>(y.get()));
    default:
	Rf_error(_("incompatible types (from %s to %s) in array subset assignment"),
		 Rf_type2char(SEXPTYPE(which%100)), Rf_type2char(SEXPTYPE(which/100)));
    }
    return nullptr;  // -Wall
}


/* Use for pairlists */
static SEXP GetOneIndex(SEXP sub, int ind)
{
    if (ind < 0 || ind+1 > Rf_length(sub))
    	Rf_error("internal error: index %d from length %d", ind, Rf_length(sub));
    if (Rf_length(sub) > 1) {
	switch (TYPEOF(sub)) {
	case INTSXP:
	    sub = Rf_ScalarInteger(INTEGER(sub)[ind]);
	    break;
	case REALSXP:
	    sub = Rf_ScalarReal(REAL(sub)[ind]);
	    break;
	case STRSXP:
	    sub = Rf_ScalarString(STRING_ELT(sub, ind));
	    break;
	default:
	    Rf_error(_("invalid subscript in list assign"));
	}
    }
    return sub;
}

/* This is only used for [[<-, so only adding one element */
static SEXP SimpleListAssign(SEXP call, SEXP x, SEXP s, SEXP y, int ind)
{
    SEXP indx, sub = CAR(s);
    int ii, n, nx;
    R_xlen_t stretch = 1;

    if (Rf_length(s) > 1)
	Rf_error(_("invalid number of subscripts to list assign"));

    PROTECT(sub = GetOneIndex(sub, ind));
    PROTECT(indx = Rf_makeSubscript(x, sub, &stretch, nullptr));

    n = Rf_length(indx);
    if (n > 1)
	Rf_error(_("invalid subscript in list assign"));

    nx = Rf_length(x);

    if (stretch) {
	SEXP t = CAR(s);
	GCStackRoot<> yi(Rf_allocList(int(stretch - nx)));
	/* This is general enough for only usage */
	if(Rf_isString(t) && Rf_length(t) == stretch - nx) {
	    SEXP z = yi;
	    int i;
	    for(i = 0; i < LENGTH(t); i++, z = CDR(z))
		SET_TAG(z, Rf_installTrChar(STRING_ELT(t, i)));
	}
	PROTECT(x = Rf_listAppend(x, yi));
	nx = int( stretch);
    }
    else PROTECT(x);

    if (n == 1) {
	ii = Rf_asInteger(indx);
	if (ii != NA_INTEGER) {
	    ii = ii - 1;
	    SEXP xi = Rf_nthcdr(x, ii % nx);
	    SETCAR(xi, y);
	}
    }
    UNPROTECT(3);
    return x;
}

/* This is for x[[s[ind]]] <- NULL */

static SEXP listRemove(SEXP x, SEXP s, int ind)
{
    std::vector<ConsCell*, Allocator<ConsCell*> > vcc;
    // Assemble vector of pointers to list elements:
    for (ConsCell* xp = SEXP_downcast<ConsCell*>(x);
	 xp; xp = xp->tail())
	vcc.push_back(xp);
    // Null out pointers to unwanted elements:
    {
	R_xlen_t stretch = 0;
	GCStackRoot<> sub(GetOneIndex(s, ind));
	GCStackRoot<IntVector>
	    iv(SEXP_downcast<IntVector*>(Rf_makeSubscript(x, sub, &stretch, nullptr)));
	size_t ns = iv->size();
	for (size_t i = 0; i < ns; ++i) {
	    int ii = (*iv)[i];
	    if (ii != NA_INTEGER) vcc[ii-1] = nullptr;
	}
    }
    // Restring the pearls:
    {
	ConsCell* ans = nullptr;
	for (size_t i = vcc.size(); i > 0; --i) {
	    ConsCell* cc = vcc[i - 1];
	    if (cc) {
		PairList* tail = static_cast<PairList*>(ans);
		ans = cc;
		ans->setTail(tail);
	    }
	}
	return ans;
    }
}


static int SubAssignArgs(PairList* args, SEXP *x, PairList** s, SEXP *y)
{
    if (CDR(args) == R_NilValue)
	Rf_error(_("SubAssignArgs: invalid number of arguments"));
    *x = args->car();
    if(CDDR(args) == R_NilValue) {
	*s = nullptr;
	*y = args->tail()->car();
	return 0;
    }
    else {
	int nsubs = 1;
	PairList* p = args->tail();
	*s = p;
	PairList* ptail = p->tail();
	while (ptail->tail()) {
	    p = ptail;
	    ptail = p->tail();
	    nsubs++;
	}
	*y = ptail->car();
	p->setTail(nullptr);
	return nsubs;
    }
}

/* The [<- operator.  "x" is the vector that is to be assigned into, */
/* y is the vector that is going to provide the new values and subs is */
/* the vector of subscripts that are going to be replaced. */
/* On entry (CAR(args)) and the last argument have been evaluated */
/* and the remainder of args have not.  If this was called directly */
/* the CAR(args) and the last arg won't have been. */

SEXP attribute_hidden do_subassign(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    const Expression* expression = SEXP_downcast<Expression*>(call);
    const BuiltInFunction* function = SEXP_downcast<BuiltInFunction*>(op);
    Environment* envx = SEXP_downcast<Environment*>(rho);
    ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::RAW);
    auto dispatched = Rf_DispatchOrEval(expression, function, &arglist, envx,
                                        MissingArgHandling::Keep);
    if (dispatched.first)
        return dispatched.second;
    return BuiltInFunction::callBuiltInWithCApi(
        do_subassign_dflt,
        expression, function, arglist, envx);
}

SEXP attribute_hidden do_subassign_dflt(SEXP call, SEXP op, SEXP argsarg,
					SEXP rho)
{
    GCStackRoot<PairList> args(SEXP_downcast<PairList*>(argsarg));

    SEXP x, y;
    PairList* subs;
    size_t nsubs = SubAssignArgs(args, &x, &subs, &y);
   
    /* If there are multiple references to an object we must */
    /* duplicate it so that only the local version is mutated. */
    /* This will duplicate more often than necessary, but saves */
    /* over always duplicating. */
    if (MAYBE_SHARED(CAR(args))) {
	x = SETCAR(args, Rf_shallow_duplicate(CAR(args)));
    }

    bool S4 = IS_S4_OBJECT(x);
    SEXPTYPE xorigtype = TYPEOF(x);
    if (xorigtype == LISTSXP || xorigtype == LANGSXP)
	x = Rf_PairToVectorList(x);

    /* bug PR#2590 coerce only if null */
    if (!x)
	x = Rf_coerceVector(x, TYPEOF(y));

    switch (TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case EXPRSXP:
    case VECSXP:
    case RAWSXP:
	{
	    VectorBase* xv = static_cast<VectorBase*>(x);
	    if (xv->size() == 0 && Rf_length(y) == 0)
		return x;
	    switch (nsubs) {
	    case 0:
		x = VectorAssign(call, rho, x, R_MissingArg, y);
		break;
	    case 1:
		x = VectorAssign(call, rho, x, subs->car(), y);
		break;
	    default:
		x = ArrayAssign(call, rho, x, subs, y);
		break;
	    }
	}
	break;
    default:
	Rf_error(R_MSG_ob_nonsub, TYPEOF(x));
	break;
    }

    if (xorigtype == LANGSXP) {
	if(Rf_length(x)) {
	    GCStackRoot<PairList> xlr(static_cast<PairList*>(VectorToPairList(x)));
	    GCStackRoot<Expression> xr(ConsCell::convert<Expression>(xlr));
	    x = xr;
	} else
	    Rf_error(_("result is zero-length and so cannot be a language object"));
    }

    /* Note the setting of NAMED(x) to zero here.  This means */
    /* that the following assignment will not duplicate the value. */
    /* This works because at this point, x is guaranteed to have */
    /* at most one symbol bound to it.  It does mean that there */
    /* will be multiple reference problems if "[<-" is used */
    /* in a naked fashion. */

    SET_NAMED(x, 0);
    if (S4)
	SET_S4_OBJECT(x);
    return x;
}

static SEXP DeleteOneVectorListItem(SEXP x, R_xlen_t which)
{
    SEXP y, xnames, ynames;
    R_xlen_t i, k, n;
    n = Rf_xlength(x);
    if (0 <= which && which < n) {
	PROTECT(y = Rf_allocVector(TYPEOF(x), n - 1));
	k = 0;
	switch (TYPEOF(x)) {
	case VECSXP:
	    for (i = 0 ; i < n; i++)
		if(i != which)
		    SET_VECTOR_ELT(y, k++, VECTOR_ELT(x, i));
	    break;
	case EXPRSXP:
	    for (i = 0 ; i < n; i++)
		if(i != which)
		    SET_XVECTOR_ELT(y, k++, XVECTOR_ELT(x, i));
	    break;
	default:
	    Rf_error(_("Internal error:"
		       " unexpected type in DeleteOneVectorListItem"));
	}
	xnames = Rf_getAttrib(x, R_NamesSymbol);
	if (xnames != R_NilValue) {
	    PROTECT(ynames = Rf_allocVector(STRSXP, n - 1));
	    k = 0;
	    for (i = 0 ; i < n; i++)
		if(i != which)
		    SET_STRING_ELT(ynames, k++, STRING_ELT(xnames, i));
	    Rf_setAttrib(y, R_NamesSymbol, ynames);
	    UNPROTECT(1);
	}
	Rf_copyMostAttrib(x, y);
	UNPROTECT(1);
	return y;
    }
    return x;
}

/* The [[<- operator; should be fast.
 *     ====
 * args[1] = object being subscripted
 * args[2] = list of subscripts
 * args[3] = replacement values */
SEXP attribute_hidden do_subassign2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    const Expression* expression = SEXP_downcast<Expression*>(call);
    const BuiltInFunction* function = SEXP_downcast<BuiltInFunction*>(op);
    Environment* envx = SEXP_downcast<Environment*>(rho);
    ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::RAW);
    auto dispatched = Rf_DispatchOrEval(expression, function, &arglist, envx,
                                        MissingArgHandling::Keep);
    if (dispatched.first)
        return dispatched.second;

    return BuiltInFunction::callBuiltInWithCApi(
        do_subassign2_dflt,
        expression, function, arglist, envx);
}

SEXP attribute_hidden
do_subassign2_dflt(SEXP call, SEXP op, SEXP argsarg, SEXP rho)
{
    PairList* args = SEXP_downcast<PairList*>(argsarg);
    SEXP dims, indx, names, newname, x, xtop, xup, y, thesub = R_NilValue, xOrig = R_NilValue;
    int i, ndims, nsubs, which, len = 0 /* -Wall */;
    R_xlen_t  stretch, offset, off = -1; /* -Wall */
    Rboolean S4, recursed=FALSE;

    PROTECT(args);

    PairList* subs;
    nsubs = SubAssignArgs(args, &x, &subs, &y);
    S4 = RHOCONSTRUCT(Rboolean, IS_S4_OBJECT(x));

    /* Handle NULL left-hand sides.  If the right-hand side */
    /* is NULL, just return the left-hand size otherwise, */
    /* convert to a zero length list (VECSXP). */

    if (Rf_isNull(x)) {
	if (Rf_isNull(y)) {
	    UNPROTECT(1);
	    return x;
	}
	if (Rf_length(y) == 1)
	    SETCAR(args, x = Rf_allocVector(TYPEOF(y), 0));
	else
	    SETCAR(args, x = Rf_allocVector(VECSXP, 0));
    }

    /* Ensure that the LHS is a local variable. */
    /* If it is not, then make a local copy. */

    if (MAYBE_SHARED(x)) {
	x = Rf_shallow_duplicate(x);
	SETCAR(args, x);
    }

    xtop = xup = x; /* x will be the element which is assigned to */

    dims = Rf_getAttrib(x, R_DimSymbol);
    ndims = Rf_length(dims);

    /* code to allow classes to extend ENVSXP */
    if(TYPEOF(x) == S4SXP) {
	xOrig = x; /* will be an S4 object */
	x = R_getS4DataSlot(x, ANYSXP);
	if(TYPEOF(x) != ENVSXP)
	  Rf_errorcall(call, _("[[<- defined for objects of type \"S4\" only for subclasses of environment"));
    }

    /* ENVSXP special case first */
    if( TYPEOF(x) == ENVSXP) {
	if( nsubs!=1 || !Rf_isString(CAR(subs)) || Rf_length(CAR(subs)) != 1 )
	    Rf_error(_("wrong args for environment subassignment"));
	Rf_defineVar(Rf_installTrChar(STRING_ELT(CAR(subs), 0)), y, x);
	UNPROTECT(1);
	return(S4 ? xOrig : x);
    }

    /* new case in 1.7.0, one vector index for a list,
       more general as of 2.10.0 */
    if (nsubs == 1) {
	thesub = CAR(subs);
	len = Rf_length(thesub); /* depth of recursion, small */
	if (len > 1) {
	    xup = Rf_vectorIndex(x, thesub, 0, len-2, /*partial ok*/TRUE, call,
			      TRUE);
	    /* OneIndex sets newname, but it will be overwritten before being used. */
	    off = Rf_OneIndex(xup, thesub, Rf_xlength(xup), 0, &newname, len-2, R_NilValue);
	    x = Rf_vectorIndex(xup, thesub, len-2, len-1, TRUE, call, TRUE);
	    recursed = TRUE;
	}
    }

    stretch = 0;
    if (Rf_isVector(x)) {
	if (!Rf_isVectorList(x) && LENGTH(y) == 0)
	    Rf_error(_("replacement has length zero"));
	if (!Rf_isVectorList(x) && LENGTH(y) > 1)
	    Rf_error(_("more elements supplied than there are to replace"));
	if (nsubs == 0 || CAR(subs) == R_MissingArg)
	    Rf_error(_("[[ ]] with missing subscript"));
	if (nsubs == 1) {
	    offset = Rf_OneIndex(x, thesub, Rf_length(x), 0, &newname,
			      recursed ? len-1 : -1, R_NilValue);
	    if (Rf_isVectorList(x) && Rf_isNull(y)) {
		x = DeleteOneVectorListItem(x, offset);
		if(recursed) {
		    if(Rf_isVectorList(xup)) SET_VECTOR_ELT(xup, off, x);
		    else xup = SimpleListAssign(call, xup, subs, x, len-2);
		}
		else xtop = x;
		UNPROTECT(1);
		return xtop;
	    }
	    if (offset < 0)
		Rf_error(_("[[ ]] subscript out of bounds"));
	    if (offset >= XLENGTH(x))
		stretch = offset + 1;
	}
	else {
	    if (ndims != nsubs)
		Rf_error(_("[[ ]] improper number of subscripts"));
	    PROTECT(indx = Rf_allocVector(INTSXP, ndims));
	    names = Rf_getAttrib(x, R_DimNamesSymbol);
	    for (i = 0; i < ndims; i++) {
		INTEGER(indx)[i] = int(
		    Rf_get1index(CAR(subs), Rf_isNull(names) ?
			      R_NilValue : VECTOR_ELT(names, i),
			      INTEGER(dims)[i],
			      /*partial ok*/FALSE, -1, call));
		subs = subs->tail();
		if (INTEGER(indx)[i] < 0 ||
		    INTEGER(indx)[i] >= INTEGER(dims)[i])
		    Rf_error(_("[[ ]] subscript out of bounds"));
	    }
	    offset = 0;
	    for (i = (ndims - 1); i > 0; i--)
		offset = (offset + INTEGER(indx)[i]) * INTEGER(dims)[i - 1];
	    offset += INTEGER(indx)[0];
	    UNPROTECT(1);
	}

	which = SubassignTypeFix(&x, &y, 2, call, rho);
	if (stretch) {
	    PROTECT(x);
	    PROTECT(y);
	    x = EnlargeVector(x, stretch);
	    UNPROTECT(2);
	}
	PROTECT(x);

	switch (which) {
	    /* as from 2.3.0 'which' is after conversion */

	case 1010:	/* logical   <- logical	  */
	case 1310:	/* integer   <- logical	  */
	/* case 1013:	   logical   <- integer	  */
	case 1313:	/* integer   <- integer	  */

	    INTEGER(x)[offset] = INTEGER(y)[0];
	    break;

	case 1410:	/* real	     <- logical	  */
	case 1413:	/* real	     <- integer	  */

	    if (INTEGER(y)[0] == NA_INTEGER)
		REAL(x)[offset] = NA_REAL;
	    else
		REAL(x)[offset] = INTEGER(y)[0];
	    break;
	/* case 1014:	   logical   <- real	  */
	/* case 1314:	   integer   <- real	  */
	case 1414:	/* real	     <- real	  */

	    REAL(x)[offset] = REAL(y)[0];
	    break;

	case 1510:	/* complex   <- logical	  */
	case 1513:	/* complex   <- integer	  */

	    if (INTEGER(y)[0] == NA_INTEGER) {
		COMPLEX(x)[offset].r = NA_REAL;
		COMPLEX(x)[offset].i = NA_REAL;
	    }
	    else {
		COMPLEX(x)[offset].r = INTEGER(y)[0];
		COMPLEX(x)[offset].i = 0.0;
	    }
	    break;

	case 1514:	/* complex   <- real	  */

	    if (ISNA(REAL(y)[0])) {
		COMPLEX(x)[offset].r = NA_REAL;
		COMPLEX(x)[offset].i = NA_REAL;
	    }
	    else {
		COMPLEX(x)[offset].r = REAL(y)[0];
		COMPLEX(x)[offset].i = 0.0;
	    }
	    break;

	/* case 1015:	   logical   <- complex	  */
	/* case 1315:	   integer   <- complex	  */
	/* case 1415:	   real	     <- complex	  */
	case 1515:	/* complex   <- complex	  */

	    COMPLEX(x)[offset] = COMPLEX(y)[0];
	    break;

	case 1610:	/* character <- logical	  */
	case 1613:	/* character <- integer	  */
	case 1614:	/* character <- real	  */
	case 1615:	/* character <- complex	  */
	case 1616:	/* character <- character */
	/* case 1016:	   logical   <- character */
	/* case 1316:	   integer   <- character */
	/* case 1416:	   real	     <- character */
	/* case 1516:	   complex   <- character */

	    SET_STRING_ELT(x, offset, STRING_ELT(y, 0));
	    break;

	case 1019:      /* logical    <- vector     */
	case 1319:      /* integer    <- vector     */
	case 1419:      /* real       <- vector     */
	case 1519:      /* complex    <- vector     */
	case 1619:      /* character  <- vector     */

	case 1901:  /* vector     <- symbol     */
	case 1902:  /* vector	  <- pairlist   */
	case 1904:  /* vector     <- environment*/
	case 1905:  /* vector     <- promise    */
	case 1906:  /* vector     <- language   */
	case 1910:  /* vector     <- logical    */
	case 1913:  /* vector     <- integer    */
	case 1914:  /* vector     <- real       */
	case 1915:  /* vector     <- complex    */
	case 1916:  /* vector     <- character  */
	case 1919:  /* vector     <- vector     */
	case 1920:  /* vector     <- expression */
	case 1921:  /* vector     <- bytecode   */
	case 1922:  /* vector     <- external pointer */
	case 1923:  /* vector     <- weak reference */
	case 1924:  /* vector     <- raw */
	case 1925:  /* vector     <- S4 */
	case 1903: case 1907: case 1908: case 1999: /* functions */

	    if( NAMED(y) ) y = duplicate(y);
	    SET_VECTOR_ELT(x, offset, y);
	    break;

	case 2002:	/* expression <- pairlist   */
	case 2006:	/* expression <- language   */
	case 2010:	/* expression <- logical    */
	case 2013:	/* expression <- integer    */
	case 2014:	/* expression <- real	    */
	case 2015:	/* expression <- complex    */
	case 2016:	/* expression <- character  */
	case 2024:      /* expression     <- raw */
	case 2025:      /* expression     <- S4 */
	case 2020:	/* expression <- expression */

	    SET_XVECTOR_ELT(x, offset, R_FixupRHS(x, y));
	    break;

	case 2424:      /* raw <- raw */

	   RAW(x)[offset] = RAW(y)[0];
	   break;

	default:
	    Rf_error(_("incompatible types (from %s to %s) in [[ assignment"),
		  Rf_type2char(RHOCONSTRUCT(SEXPTYPE, which%100)), Rf_type2char(RHOCONSTRUCT(SEXPTYPE, which/100)));
	}
	/* If we stretched, we may have a new name. */
	/* In this case we must create a names attribute */
	/* (if it doesn't already exist) and set the new */
	/* value in the names attribute. */
	if (stretch && newname != R_NilValue) {
	    names = Rf_getAttrib(x, R_NamesSymbol);
	    if (names == R_NilValue) {
		PROTECT(names = Rf_allocVector(STRSXP, Rf_length(x)));
		SET_STRING_ELT(names, offset, newname);
		Rf_setAttrib(x, R_NamesSymbol, names);
		UNPROTECT(1);
	    }
	    else
		SET_STRING_ELT(names, offset, newname);
	}
	UNPROTECT(1);
    }
    else if (Rf_isPairList(x)) {
	y = R_FixupRHS(x, y);
	PROTECT(y);
	if (nsubs == 1) {
	    if (Rf_isNull(y)) {
		x = listRemove(x, CAR(subs), len-1);
	    }
	    else {
		x = SimpleListAssign(call, x, subs, y, len-1);
	    }
	}
	else {
	    if (ndims != nsubs)
		Rf_error(_("[[ ]] improper number of subscripts"));
	    PROTECT(indx = Rf_allocVector(INTSXP, ndims));
	    names = Rf_getAttrib(x, R_DimNamesSymbol);
	    for (i = 0; i < ndims; i++) {
		INTEGER(indx)[i] = int(
		    Rf_get1index(CAR(subs), VECTOR_ELT(names, i),
			      INTEGER(dims)[i],
			      /*partial ok*/FALSE, -1, call));
		subs = subs->tail();
		if (INTEGER(indx)[i] < 0 ||
		    INTEGER(indx)[i] >= INTEGER(dims)[i])
		    Rf_error(_("[[ ]] subscript (%d) out of bounds"), i+1);
	    }
	    offset = 0;
	    for (i = (ndims - 1); i > 0; i--)
		offset = (offset + INTEGER(indx)[i]) * INTEGER(dims)[i - 1];
	    offset += INTEGER(indx)[0];
	    SEXP slot = Rf_nthcdr(x, (int) offset);
	    SETCAR(slot, Rf_duplicate(y));
	    /* FIXME: add name */
	    UNPROTECT(1);
	}
	UNPROTECT(1);
    }
    else Rf_error(R_MSG_ob_nonsub, type2char(TYPEOF(x)));

    if(recursed) {
	if (Rf_isVectorList(xup)) {
	    SET_VECTOR_ELT(xup, off, x);
	} else {
	    xup = SimpleListAssign(call, xup, subs, x, len-2);
	}
	if (len == 2)
	    xtop = xup;
    }
    else xtop = x;

    UNPROTECT(1);
    SET_NAMED(xtop, 0);
    if(S4) SET_S4_OBJECT(xtop);
    return xtop;
}

/* $<-(x, elt, val), and elt does not get evaluated it gets matched.
   to get DispatchOrEval to work we need to first translate it
   to a string
*/
SEXP attribute_hidden do_subassign3(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP nlist = nullptr;

    ArgList arglist(SEXP_downcast<PairList*>(Rf_fixSubset3Args(call, args, env, &nlist)), ArgList::RAW);

    auto dispatched = Rf_DispatchOrEval(SEXP_downcast<Expression*>(call),
                                        SEXP_downcast<BuiltInFunction*>(op),
                                        &arglist,
                                        SEXP_downcast<Environment*>(env),
                                        MissingArgHandling::Keep);
    if (dispatched.first)
        return dispatched.second;

    if (nlist == R_NilValue)
	nlist = Rf_installTrChar(STRING_ELT(arglist.get(1), 0));

    return R_subassign3_dflt(call, arglist.get(0), nlist, arglist.get(2));
}

/* used in "$<-" (above) and methods_list_dispatch.c */
SEXP R_subassign3_dflt(SEXP call, SEXP x, SEXP nlist, SEXP val)
{
    SEXP t;
    PROTECT_INDEX pvalidx, pxidx;
    Rboolean maybe_duplicate=FALSE;
    Rboolean S4; SEXP xS4 = R_NilValue;

    PROTECT_WITH_INDEX(x, &pxidx);
    PROTECT_WITH_INDEX(val, &pvalidx);
    S4 = RHOCONSTRUCT(Rboolean, IS_S4_OBJECT(x));

    if (MAYBE_SHARED(x)) {
	x = Rf_shallow_duplicate(x);
	REPROTECT(x, pxidx);
    }
    /* If we aren't creating a new entry and NAMED>0
       we need to duplicate to prevent cycles.
       If we are creating a new entry we could duplicate
       or increase NAMED. We duplicate if NAMED == 1, but
       not if NAMED > 1 */
    if (MAYBE_SHARED(val))
	maybe_duplicate=TRUE;
    else if (MAYBE_REFERENCED(val))
	REPROTECT(val = R_FixupRHS(x, val), pvalidx);
    /* code to allow classes to extend ENVSXP */
    if(TYPEOF(x) == S4SXP) {
	xS4 = x;
        x = R_getS4DataSlot(x, ANYSXP);
	if(x == R_NilValue)
	  Rf_errorcall(call, _("no method for assigning subsets of this S4 class"));
    }

    if ((Rf_isList(x) || Rf_isLanguage(x)) && !Rf_isNull(x)) {
	/* Here we do need to duplicate */
	if (maybe_duplicate)
	    REPROTECT(val = R_FixupRHS(x, val), pvalidx);
	if (TAG(x) == nlist) {
	    if (val == R_NilValue) {
		SET_ATTRIB(CDR(x), ATTRIB(x));
		IS_S4_OBJECT(x) ?  SET_S4_OBJECT(CDR(x)) : UNSET_S4_OBJECT(CDR(x));
		SET_NAMED(CDR(x), NAMED(x));
		x = CDR(x);
	    }
	    else
		SETCAR(x, val);
	}
	else {
	    for (t = x; t != R_NilValue; t = CDR(t))
		if (TAG(CDR(t)) == nlist) {
		    if (val == R_NilValue)
			SETCDR(t, CDDR(t));
		    else
			SETCAR(CDR(t), val);
		    break;
		}
		else if (CDR(t) == R_NilValue && val != R_NilValue) {
		    SETCDR(t, new PairList);
		    SET_TAG(CDR(t), nlist);
		    SETCADR(t, val);
		    break;
		}
	}
	if (x == R_NilValue && val != R_NilValue) {
	    x = Rf_allocList(1);
	    SETCAR(x, val);
	    SET_TAG(x, nlist);
	}
    }
    /* cannot use isEnvironment since we do not want NULL here */
    else if( TYPEOF(x) == ENVSXP ) {
	Rf_defineVar(nlist, val, x);
    }
    else if( TYPEOF(x) == SYMSXP || /* Used to 'work' in R < 2.8.0 */
	     TYPEOF(x) == CLOSXP ||
	     TYPEOF(x) == SPECIALSXP ||
	     TYPEOF(x) == BUILTINSXP) {
	Rf_error(R_MSG_ob_nonsub, Rf_type2char(TYPEOF(x)));
    }
    else {
	R_xlen_t i, imatch, nx;
	SEXP names;
	SEXPTYPE type = VECSXP;

	if (Rf_isExpression(x))
	    type = EXPRSXP;
	else if (!Rf_isNewList(x)) {
	    Rf_warning(_("Coercing LHS to a list"));
	    REPROTECT(x = Rf_coerceVector(x, VECSXP), pxidx);
	}
	names = Rf_getAttrib(x, R_NamesSymbol);
	nx = Rf_xlength(x);
	nlist = PRINTNAME(nlist);
	if (Rf_isNull(val)) {
	    /* If "val" is NULL, this is an element deletion */
	    /* if there is a match to "nlist" otherwise "x" */
	    /* is unchanged.  The attributes need adjustment. */
	    if (names != R_NilValue) {
		imatch = -1;
		for (i = 0; i < nx; i++)
		    if (Rf_NonNullStringMatch(STRING_ELT(names, i), nlist)) {
			imatch = i;
			break;
		    }
		if (imatch >= 0) {
		    SEXP ans, ansnames;
		    int ii;
		    PROTECT(ans = Rf_allocVector(type, nx - 1));
		    PROTECT(ansnames = Rf_allocVector(STRSXP, nx - 1));
		    for (i = 0, ii = 0; i < nx; i++)
			if (i != imatch) {
			    if (type == EXPRSXP)
				SET_XVECTOR_ELT(ans, ii, XVECTOR_ELT(x, i));
			    else SET_VECTOR_ELT(ans, ii, VECTOR_ELT(x, i));
			    SET_STRING_ELT(ansnames, ii, STRING_ELT(names, i));
			    ii++;
			}
		    Rf_setAttrib(ans, R_NamesSymbol, ansnames);
		    Rf_copyMostAttrib(x, ans);
		    UNPROTECT(2);
		    x = ans;
		}
		/* else x is unchanged */
	    }
	}
	else {
	    /* If "val" is non-NULL, we are either replacing */
	    /* an existing list element or we are adding a new */
	    /* element. */
	    imatch = -1;
	    if (!Rf_isNull(names)) {
		for (i = 0; i < nx; i++)
		    if (NonNullStringMatch(STRING_ELT(names, i), nlist)) {
			imatch = i;
			break;
		    }
	    }
	    if (imatch >= 0) {
		/* We are just replacing an element */
		if (maybe_duplicate)
		    REPROTECT(val = R_FixupRHS(x, val), pvalidx);
		SET_VECTOR_ELT(x, imatch, val);
	    }
	    else {
		/* We are introducing a new element (=> *no* duplication) */
		/* Enlarge the list, add the new element */
		/* and finally, adjust the attributes. */
		SEXP ans, ansnames;
		PROTECT(ans = allocVector(VECSXP, nx + 1));
		PROTECT(ansnames = allocVector(STRSXP, nx + 1));
		for (i = 0; i < nx; i++)
		    SET_VECTOR_ELT(ans, i, VECTOR_ELT(x, i));
		if (isNull(names)) {
		    for (i = 0; i < nx; i++)
			SET_STRING_ELT(ansnames, i, R_BlankString);
		}
		else {
		    for (i = 0; i < nx; i++)
			SET_STRING_ELT(ansnames, i, STRING_ELT(names, i));
		}
		SET_VECTOR_ELT(ans, nx, val);
		SET_STRING_ELT(ansnames, nx,  nlist);
		setAttrib(ans, R_NamesSymbol, ansnames);
		copyMostAttrib(x, ans);
		UNPROTECT(2);
		x = ans;
	    }
	}
    }
    UNPROTECT(2);
    if(xS4 != R_NilValue)
	x = xS4; /* x was an env't, the data slot of xS4 */
    SET_NAMED(x, 0);
    if(S4) SET_S4_OBJECT(x);
    return x;
}
