/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997-2017   The R Core Team
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

#define R_NO_REMAP

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Localization.h>
#include <Internal.h>
#include <R_ext/RS.h> /* for test of S4 objects */
#include <rho/ComplexVector.hpp>
#include <rho/ExpressionVector.hpp>
#include <rho/GCStackRoot.hpp>
#include <rho/Promise.hpp>
#include <rho/RawVector.hpp>
#include <rho/Subscripting.hpp>
#include <rho/unrho.hpp>

using namespace rho;

/* The SET_STDVEC_LENGTH macro is used to modify the length of
   growable vectors. This would need to change to allow ALTREP vectors to
   grow in place.

   SETLENGTH is used when checking the write barrier.
   Always using SETLENGTH would be OK but maybe a little less efficient. */
#ifndef SET_STDVEC_LENGTH
# define SET_STDVEC_LENGTH(x, v) SETLENGTH(x, v)
#endif

R_INLINE static SEXP getNames(SEXP x)
{
    /* defer to getAttrib if a 'dim' attribute is present */
    for (SEXP attr = ATTRIB(x); attr != nullptr; attr = CDR(attr))
	if (TAG(attr) == Symbols::DimSymbol)
	    return Rf_getAttrib(x, Symbols::NamesSymbol);

    /* don't use getAttrib since that would mark as immutable */
    for (SEXP attr = ATTRIB(x); attr != nullptr; attr = CDR(attr))
	if (TAG(attr) == Symbols::NamesSymbol)
	    return CAR(attr);

    return nullptr;
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
    static SEXP R_CheckBoundsSymbol = nullptr;

    if (R_CheckBoundsSymbol == nullptr)
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
	XTRUELENGTH(x) >= newlen) {
	SET_STDVEC_LENGTH(x, newlen);
	names = getNames(x);
	if (!Rf_isNull(names)) {
	    newnames = EnlargeNames(names, len, newlen);
	    if (names != newnames)
		Rf_setAttrib(x, Symbols::NamesSymbol, newnames);
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
	expand = envval != nullptr ? atof(envval) : expand_dflt;
	if (expand < 1 || expand > 2) {
	    expand = expand_dflt;
	    Rf_error("bad expand value");
	}
    }

    if (newlen > len) {
	double expanded_nlen = newlen * expand;
	if (expanded_nlen <= double(R_XLEN_T_MAX))
	    newtruelen = (R_xlen_t) expanded_nlen;
	else
	    newtruelen = newlen;
    }
    else
	/* sometimes this is called when no expansion is needed */
	newtruelen = newlen;

    /**** for now, don't cross the long vector boundary; drop when
	  ALTREP is merged */
/*
#ifdef ALTREP
#error drop the limitation to short vectors
#endif

    if (newtruelen > R_LEN_T_MAX) newtruelen = newlen;
*/
    PROTECT(x);
    PROTECT(newx = Rf_allocVector(TYPEOF(x), newtruelen));

    /* Copy the elements into place. */
    switch(TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
	for (R_xlen_t i = 0; i < len; i++)
	    INTEGER0(newx)[i] = INTEGER_ELT(x, i);
	for (R_xlen_t i = len; i < newtruelen; i++)
	    INTEGER0(newx)[i] = R_NaInt;
	break;
    case REALSXP:
	for (R_xlen_t i = 0; i < len; i++)
	    REAL0(newx)[i] = REAL_ELT(x, i);
	for (R_xlen_t i = len; i < newtruelen; i++)
	    REAL0(newx)[i] = R_NaReal;
	break;
    case CPLXSXP:
	for (R_xlen_t i = 0; i < len; i++)
	    COMPLEX0(newx)[i] = COMPLEX_ELT(x, i);
	for (R_xlen_t i = len; i < newtruelen; i++) {
	    COMPLEX0(newx)[i].r = R_NaReal;
	    COMPLEX0(newx)[i].i = R_NaReal;
	}
	break;
    case STRSXP:
	for (R_xlen_t i = 0; i < len; i++)
	    SET_STRING_ELT(newx, i, STRING_ELT(x, i));
	for (R_xlen_t i = len; i < newtruelen; i++)
	    SET_STRING_ELT(newx, i, R_NaString); /* was R_BlankString  < 1.6.0 */
	break;
    case EXPRSXP:
	for (R_xlen_t i = 0; i < len; i++)
	    SET_XVECTOR_ELT(newx, i, XVECTOR_ELT(x, i));
	for (R_xlen_t i = len; i < newtruelen; i++)
	    SET_XVECTOR_ELT(newx, i, nullptr);
	break;
    case VECSXP:
	for (R_xlen_t i = 0; i < len; i++)
	    SET_VECTOR_ELT(newx, i, VECTOR_ELT(x, i));
	for (R_xlen_t i = len; i < newtruelen; i++)
	    SET_VECTOR_ELT(newx, i, nullptr);
	break;
    case RAWSXP:
	for (R_xlen_t i = 0; i < len; i++)
	    RAW0(newx)[i] = RAW_ELT(x, i);
	for (R_xlen_t i = len; i < newtruelen; i++)
	    RAW0(newx)[i] = Rbyte(0);
	break;
    default:
	UNIMPLEMENTED_TYPE("EnlargeVector", x);
    }
    if (newlen < newtruelen) {
	//SET_GROWABLE_BIT(newx);
	SET_TRUELENGTH(newx, newtruelen);
	SET_STDVEC_LENGTH(newx, newlen);
    }

    /* Adjust the attribute list. */
    names = getNames(x);
    if (!Rf_isNull(names))
	Rf_setAttrib(newx, Symbols::NamesSymbol, EnlargeNames(names, len, newlen));
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
    static GCRoot<BuiltInFunction> op = nullptr;
    if (op == nullptr)
        op = SEXP_downcast<BuiltInFunction*>(INTERNAL(Rf_install("as.vector")));

    static GCRoot<> any = Rf_mkString("any");
    ArgList args({ x, any }, ArgList::RAW);
    return Rf_DispatchOrEval(call, op, &args, rho, MissingArgHandling::Keep);
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
	      Rf_type2char(SEXPTYPE(which%100)), Rf_type2char(SEXPTYPE(which/100)));
    }

    return(100 * TYPEOF(*x) + TYPEOF(*y));
}

#ifdef LONG_VECTOR_SUPPORT
R_INLINE static R_xlen_t gi(SEXP indx, R_xlen_t i)
{
    if (TYPEOF(indx) == REALSXP) {
	double d = REAL_ELT(indx, i);
	return std::isfinite(d) ? R_xlen_t(d) : R_NaInt;
    } else
	return INTEGER_ELT(indx, i);
}
#else
#define R_SHORT_LEN_MAX INT_MAX
R_INLINE static int gi(SEXP indx, R_xlen_t i)
{
    if (TYPEOF(indx) == REALSXP) {
	double d = REAL_ELT(indx, i);
	if (!std::isfinite(d) || d < -R_SHORT_LEN_MAX || d > R_SHORT_LEN_MAX) return R_NaInt;
	return int(d);
    } else
	return INTEGER_ELT(indx, i);
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
    int *pinclude = INTEGER0(include);
    for (i = 0; i < len; i++)
	pinclude[i] = 1;
    for (i = 0; i < lenw; i++) {
	ii = gi(which, i);
	if (0 < ii  && ii <= len)
	    pinclude[ii - 1] = 0;
    }
    ii = 0;
    for (i = 0; i < len; i++)
	ii += pinclude[i];
    if (ii == len) {
	UNPROTECT(1);
	return x;
    }
    PROTECT(xnew = Rf_allocVector(TYPEOF(x), ii));
    ii = 0;
    switch (TYPEOF(x)) {
    case VECSXP:
	for (i = 0; i < len; i++) {
	    if (pinclude[i] == 1) {
		SET_VECTOR_ELT(xnew, ii, VECTOR_ELT(x, i));
		ii++;
	    }
	}
	break;
    case EXPRSXP:
	for (i = 0; i < len; i++) {
	    if (pinclude[i] == 1) {
		SET_XVECTOR_ELT(xnew, ii, XVECTOR_ELT(x, i));
		ii++;
	    }
	}
	break;
    default:
	Rf_error(_("Internal error: unexpected type in DeleteListElements"));
    }
    xnames = Rf_getAttrib(x, Symbols::NamesSymbol);
    if (xnames != nullptr) {
	PROTECT(xnewnames = Rf_allocVector(STRSXP, ii));
	ii = 0;
	for (i = 0; i < len; i++) {
	    if (pinclude[i] == 1) {
		SET_STRING_ELT(xnewnames, ii, STRING_ELT(xnames, i));
		ii++;
	    }
	}
	Rf_setAttrib(xnew, Symbols::NamesSymbol, xnewnames);
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
	return nullptr;
    }

    /* Check to see if we have special matrix subscripting. */
    /* If so, we manufacture a real subscript vector. */

    PROTECT(s);
    if (ATTRIB(s) != nullptr) { /* pretest to speed up simple case */
	SEXP dim = Rf_getAttrib(x, Symbols::DimSymbol);
	if (Rf_isMatrix(s) && Rf_isArray(x) && Rf_ncols(s) == Rf_length(dim)) {
	    if (Rf_isString(s)) {
		s = Rf_strmat2intmat(s, Rf_GetArrayDimnames(x), call);
		UNPROTECT(1);
		PROTECT(s);
	    }
	    if (Rf_isInteger(s) || Rf_isReal(s)) {
		s = Rf_mat2indsub(dim, s, nullptr);
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
	return Subscripting::vectorSubassign(SEXP_downcast<LogicalVector*>(x.get()), s,
					     static_cast<const LogicalVector*>(y.get()));
    /* case 1013:  logical   <- integer	  */
    /* case 1014:  logical   <- real	  */
    /* case 1015:  logical   <- complex	  */
    /* case 1016:  logical   <- character */
    /* case 1019:  logical   <- vector   */
    case 1310:	/* integer   <- logical	  */
	return Subscripting::vectorSubassign(SEXP_downcast<IntVector*>(x.get()), s,
					     static_cast<const LogicalVector*>(y.get()));
    case 1313:	/* integer   <- integer	  */
	return Subscripting::vectorSubassign(SEXP_downcast<IntVector*>(x.get()), s,
					     static_cast<const IntVector*>(y.get()));
    /* case 1314:  integer   <- real	  */
    /* case 1315:  integer   <- complex	  */
    /* case 1316:  integer   <- character */
    /* case 1319:  integer    <- vector   */
    case 1410:	/* real	     <- logical	  */
	return Subscripting::vectorSubassign(SEXP_downcast<RealVector*>(x.get()), s,
					     static_cast<const LogicalVector*>(y.get()));
    case 1413:	/* real	     <- integer	  */
	return Subscripting::vectorSubassign(SEXP_downcast<RealVector*>(x.get()), s,
					     static_cast<const IntVector*>(y.get()));
    case 1414:	/* real	     <- real	  */
	return Subscripting::vectorSubassign(SEXP_downcast<RealVector*>(x.get()), s,
					     static_cast<const RealVector*>(y.get()));
    /* case 1415:  real	     <- complex	  */
    /* case 1416:  real	     <- character */
    /* case 1419:  real       <- vector   */
    case 1510:	/* complex   <- logical	  */
	return Subscripting::vectorSubassign(SEXP_downcast<ComplexVector*>(x.get()), s,
					     static_cast<const LogicalVector*>(y.get()));
    case 1513:	/* complex   <- integer	  */
	return Subscripting::vectorSubassign(SEXP_downcast<ComplexVector*>(x.get()), s,
					     static_cast<const IntVector*>(y.get()));
    case 1514:	/* complex   <- real	  */
	return Subscripting::vectorSubassign(SEXP_downcast<ComplexVector*>(x.get()), s,
					     static_cast<const RealVector*>(y.get()));
    case 1515:	/* complex   <- complex	  */
	return Subscripting::vectorSubassign(SEXP_downcast<ComplexVector*>(x.get()), s,
					     static_cast<const ComplexVector*>(y.get()));
    /* case 1516:  complex   <- character */
    /* case 1519:  complex    <- vector   */
    /* case 1610:  character <- logical	  */
    /* case 1613:  character <- integer	  */
    /* case 1614:  character <- real	  */
    /* case 1615:  character <- complex	  */
    case 1616:	/* character <- character */
	return Subscripting::vectorSubassign(SEXP_downcast<StringVector*>(x.get()), s,
					     static_cast<const StringVector*>(y.get()));
    /* case 1619:  character  <- vector   */

    /* case 1910:  vector     <- logical    */
    /* case 1913:  vector     <- integer    */
    /* case 1914:  vector     <- real       */
    /* case 1915:  vector     <- complex    */
    /* case 1916:  vector     <- character  */

    case 1919:  /* vector     <- vector     */
	return Subscripting::vectorSubassign(SEXP_downcast<ListVector*>(x.get()), s,
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
	return Subscripting::vectorSubassign(SEXP_downcast<ExpressionVector*>(x.get()), s,
					     static_cast<const ListVector*>(y.get()));
    case 2020:	/* expression <- expression */
	return Subscripting::vectorSubassign(SEXP_downcast<ExpressionVector*>(x.get()), s,
					     static_cast<const ExpressionVector*>(y.get()));
    case 1900:  /* vector     <- null       */
    case 2000:  /* expression <- null       */
	{
	    R_xlen_t stretch = 1;
	    GCStackRoot<> indx(Rf_makeSubscript(x, s, &stretch, nullptr));
	    return DeleteListElements(x, indx);
	}
    case 2424:	/* raw   <- raw	  */
	return Subscripting::vectorSubassign(SEXP_downcast<RawVector*>(x.get()), s,
					     static_cast<const RawVector*>(y.get()));
    default:
	if (Rf_xlength(y) == 0) {
	    if (y != nullptr
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
	return Subscripting::arraySubassign(SEXP_downcast<LogicalVector*>(x.get()),
					    subscripts,
					    SEXP_downcast<LogicalVector*>(y.get()));
    /* case 1013:  logical   <- integer	  */
    /* case 1014:  logical   <- real	  */
    /* case 1015:  logical   <- complex	  */
    /* case 1016:  logical   <- character */
    case 1310:	/* integer   <- logical	  */
	return Subscripting::arraySubassign(SEXP_downcast<IntVector*>(x.get()),
					    subscripts,
					    SEXP_downcast<LogicalVector*>(y.get()));
    case 1313:	/* integer   <- integer	  */
	return Subscripting::arraySubassign(SEXP_downcast<IntVector*>(x.get()),
					    subscripts,
					    SEXP_downcast<IntVector*>(y.get()));
    /* case 1314:  integer   <- real	  */
    /* case 1315:  integer   <- complex	  */
    /* case 1316:  integer   <- character */
    case 1410:	/* real	     <- logical	  */
	return Subscripting::arraySubassign(SEXP_downcast<RealVector*>(x.get()),
					    subscripts,
					    SEXP_downcast<LogicalVector*>(y.get()));
    case 1413:	/* real	     <- integer	  */
	return Subscripting::arraySubassign(SEXP_downcast<RealVector*>(x.get()),
					    subscripts,
					    SEXP_downcast<IntVector*>(y.get()));
    case 1414:	/* real	     <- real	  */
	return Subscripting::arraySubassign(SEXP_downcast<RealVector*>(x.get()),
					    subscripts,
					    SEXP_downcast<RealVector*>(y.get()));
    /* case 1415:  real	     <- complex	  */
    /* case 1416:  real	     <- character */
    case 1510:	/* complex   <- logical	  */
	return Subscripting::arraySubassign(SEXP_downcast<ComplexVector*>(x.get()),
					    subscripts,
					    SEXP_downcast<LogicalVector*>(y.get()));
    case 1513:	/* complex   <- integer	  */
	return Subscripting::arraySubassign(SEXP_downcast<ComplexVector*>(x.get()),
					    subscripts,
					    SEXP_downcast<IntVector*>(y.get()));
    case 1514:	/* complex   <- real	  */
	return Subscripting::arraySubassign(SEXP_downcast<ComplexVector*>(x.get()),
					    subscripts,
					    SEXP_downcast<RealVector*>(y.get()));
    case 1515:	/* complex   <- complex	  */
	return Subscripting::arraySubassign(SEXP_downcast<ComplexVector*>(x.get()),
					    subscripts,
					    SEXP_downcast<ComplexVector*>(y.get()));
    /* case 1516:  complex   <- character */
    /* case 1610:  character <- logical	  */
    /* case 1613:  character <- integer	  */
    /* case 1614:  character <- real	  */
    /* case 1615:  character <- complex	  */
    case 1616:	/* character <- character */
	return Subscripting::arraySubassign(SEXP_downcast<StringVector*>(x.get()),
					    subscripts,
					    SEXP_downcast<StringVector*>(y.get()));

    case 1919: /* vector <- vector */
	return Subscripting::arraySubassign(SEXP_downcast<ListVector*>(x.get()),
					    subscripts,
					    SEXP_downcast<ListVector*>(y.get()));
    case 2424: /* raw <- raw */
	return Subscripting::arraySubassign(SEXP_downcast<RawVector*>(x.get()),
					    subscripts,
					    SEXP_downcast<RawVector*>(y.get()));
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
	    sub = Rf_ScalarInteger(INTEGER_ELT(sub, ind));
	    break;
	case REALSXP:
	    sub = Rf_ScalarReal(REAL_ELT(sub, ind));
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
	nx = int(stretch);
    }
    else PROTECT(x);

    if (n == 1) {
	ii = Rf_asInteger(indx);
	if (ii != R_NaInt) {
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
	    if (ii != R_NaInt) vcc[ii-1] = nullptr;
	}
    }
    // Restring the pearls:
    {
	ConsCell* ans = nullptr;
	for (size_t i = vcc.size(); i > 0; --i) {
	    ConsCell* cc = vcc[i - 1];
	    if (cc) {
		PairList* tail = SEXP_downcast<PairList*>(ans);
		ans = cc;
		ans->setTail(tail);
	    }
	}
	return ans;
    }
}


// For  x[s] <- y  --- extract (x, s, y)  and return the number of indices
R_INLINE static int SubAssignArgs(PairList* args, SEXP *x, PairList** s, SEXP *y)
{
    if (CDR(args) == nullptr)
	Rf_error(_("SubAssignArgs: invalid number of arguments"));
    *x = args->car();
    if(CDDR(args) == nullptr) {
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

HIDDEN SEXP do_subassign(SEXP call, SEXP op, SEXP args, SEXP rho)
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

HIDDEN SEXP do_subassign_dflt(SEXP call, SEXP op, SEXP argsarg,
					SEXP rho)
{
    GCStackRoot<PairList> args(SEXP_downcast<PairList*>(argsarg));

    SEXP x, y;
    PairList* subs;
    size_t nsubs = SubAssignArgs(args, &x, &subs, &y);

    /* make sure the LHS is duplicated if it matches one of the indices */
    /* otherwise this gets the wrong answer:
          permute <- structure(c(3L, 1L, 2L), dim = c(3, 1))
	  permute[permute, 1] <- 1:3
	  as.vector(permute)
    */
    for (SEXP s = subs; s != nullptr; s = CDR(s)) {
	SEXP idx = CAR(s);
	if (x == idx)
	    MARK_NOT_MUTABLE(x);
    }

    /* If there are multiple references to an object we must */
    /* duplicate it so that only the local version is mutated. */
    /* This will duplicate more often than necessary, but saves */
    /* over always duplicating. */

    if (MAYBE_SHARED(CAR(args))) {
	x = SETCAR(args, Rf_shallow_duplicate(CAR(args)));
    }

    bool S4 = IS_S4_OBJECT(x);

    SEXPTYPE xorigtype = TYPEOF(x);
    if (xorigtype == LISTSXP || xorigtype == LANGSXP) {
	x = Rf_PairToVectorList(x);
    } else if (Rf_xlength(x) == 0) {
	if (Rf_xlength(y) == 0 && (Rf_isNull(x) || TYPEOF(x) == TYPEOF(y) ||
				// isVectorList(y):
				TYPEOF(y) == VECSXP || TYPEOF(y) == EXPRSXP)) {
	    return (x);
	} else {
	    /* bug PR#2590 coerce only if null */
	    if (!x)
		x = Rf_coerceVector(x, TYPEOF(y));
	}
    }

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
	    VectorBase* xv = SEXP_downcast<VectorBase*>(x);
	    if (xv->size() == 0 && Rf_length(y) == 0 && (Rf_isNull(x) || TYPEOF(x) == TYPEOF(y)))
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
	Rf_error(_("object of type '%s' is not subsettable"), TYPEOF(x));
	break;
    }

    if (xorigtype == LANGSXP) {
	if(Rf_length(x)) {
	    GCStackRoot<PairList> xlr(SEXP_downcast<PairList*>(Rf_VectorToPairList(x)));
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

    SETTER_CLEAR_NAMED(x);
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
	    Rf_error(_("Internal error: unexpected type in DeleteOneVectorListItem"));
	}
	xnames = Rf_getAttrib(x, Symbols::NamesSymbol);
	if (xnames != nullptr) {
	    PROTECT(ynames = Rf_allocVector(STRSXP, n - 1));
	    k = 0;
	    for (i = 0 ; i < n; i++)
		if(i != which)
		    SET_STRING_ELT(ynames, k++, STRING_ELT(xnames, i));
	    Rf_setAttrib(y, Symbols::NamesSymbol, ynames);
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
 * args[1] =: x    = object being subscripted
 * args[2] =: subs = list of subscripts
 * args[3] =: y    = replacement values */
HIDDEN SEXP do_subassign2(SEXP call, SEXP op, SEXP args, SEXP rho)
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

HIDDEN SEXP
do_subassign2_dflt(SEXP call, SEXP op, SEXP argsarg, SEXP rho)
{
    PairList* args = SEXP_downcast<PairList*>(argsarg);
    SEXP dims, indx, names, newname, x, xtop, xup, y, thesub = nullptr, xOrig = nullptr;
    int i, ndims, nsubs, which, len = 0 /* -Wall */;
    R_xlen_t  stretch, offset, off = -1; /* -Wall */
    Rboolean S4, recursed=FALSE;

    PROTECT(args);

    PairList* subs;
    nsubs = SubAssignArgs(args, &x, &subs, &y);
    S4 = Rboolean(IS_S4_OBJECT(x));

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

    /* code to allow classes to extend ENVSXP */
    if(TYPEOF(x) == S4SXP) {
	xOrig = x; /* will be an S4 object */
	x = R_getS4DataSlot(x, ANYSXP);
	if(TYPEOF(x) != ENVSXP)
	  Rf_errorcall(call, _("[[<- defined for objects of type \"S4\" only for subclasses of environment"));
    }

    xtop = xup = x; /* x will be the element which is assigned to */

    dims = Rf_getAttrib(x, Symbols::DimSymbol);
    ndims = Rf_length(dims);

    int *pdims = nullptr;
    if (ndims > 0) {
	if (TYPEOF(dims) == INTSXP)
	    pdims = INTEGER(dims);
	else
	    Rf_error(_("improper dimensions"));
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
	    xup = vectorIndex(x, thesub, 0, len-2, /*partial ok*/TRUE, call,
			      TRUE);
	    /* OneIndex sets newname, but it will be overwritten before being used. */
	    off = Rf_OneIndex(xup, thesub, Rf_xlength(xup), 0, &newname, len-2, nullptr);
	    x = vectorIndex(xup, thesub, len-2, len-1, TRUE, call, TRUE);
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
			      recursed ? len-1 : -1, nullptr);
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
	    int *pindx = INTEGER0(indx);
	    names = Rf_getAttrib(x, Symbols::DimNamesSymbol);
	    for (i = 0; i < ndims; i++) {
		pindx[i] = int(
		    Rf_get1index(CAR(subs), Rf_isNull(names) ?
			      nullptr : VECTOR_ELT(names, i),
			      pdims[i],
			      /*partial ok*/FALSE, -1, call));
		subs = subs->tail();
		if (pindx[i] < 0 ||
		    pindx[i] >= pdims[i])
		    Rf_error(_("[[ ]] subscript out of bounds"));
	    }
	    offset = 0;
	    for (i = (ndims - 1); i > 0; i--)
		offset = (offset + pindx[i]) * pdims[i - 1];
	    offset += pindx[0];
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

	    INTEGER(x)[offset] = INTEGER_ELT(y, 0);
	    break;

	case 1410:	/* real	     <- logical	  */
	case 1413:	/* real	     <- integer	  */

	    if (INTEGER_ELT(y, 0) == R_NaInt)
		REAL(x)[offset] = R_NaReal;
	    else
		REAL(x)[offset] = INTEGER_ELT(y, 0);
	    break;
	/* case 1014:	   logical   <- real	  */
	/* case 1314:	   integer   <- real	  */
	case 1414:	/* real	     <- real	  */

	    REAL(x)[offset] = REAL(y)[0];
	    break;

	case 1510:	/* complex   <- logical	  */
	case 1513:	/* complex   <- integer	  */

	    if (INTEGER_ELT(y, 0) == R_NaInt) {
		COMPLEX(x)[offset].r = R_NaReal;
		COMPLEX(x)[offset].i = R_NaReal;
	    }
	    else {
		COMPLEX(x)[offset].r = INTEGER_ELT(y, 0);
		COMPLEX(x)[offset].i = 0.0;
	    }
	    break;

	case 1514:	/* complex   <- real	  */

	    if (ISNA(REAL_ELT(y, 0))) {
		COMPLEX(x)[offset].r = R_NaReal;
		COMPLEX(x)[offset].i = R_NaReal;
	    }
	    else {
		COMPLEX(x)[offset].r = REAL_ELT(y, 0);
		COMPLEX(x)[offset].i = 0.0;
	    }
	    break;

	/* case 1015:	   logical   <- complex	  */
	/* case 1315:	   integer   <- complex	  */
	/* case 1415:	   real	     <- complex	  */
	case 1515:	/* complex   <- complex	  */

	    COMPLEX(x)[offset] = COMPLEX_ELT(y, 0);
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

	    if( NAMED(y) ) y = Rf_duplicate(y);
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

	    RAW(x)[offset] = RAW_ELT(y, 0);
	    break;

	default:
	    Rf_error(_("incompatible types (from %s to %s) in [[ assignment"),
		  Rf_type2char(SEXPTYPE(which%100)), Rf_type2char(SEXPTYPE(which/100)));
	}
	/* If we stretched, we may have a new name. */
	/* In this case we must create a names attribute */
	/* (if it doesn't already exist) and set the new */
	/* value in the names attribute. */
	if (stretch && newname != nullptr) {
	    names = Rf_getAttrib(x, Symbols::NamesSymbol);
	    if (names == nullptr) {
		PROTECT(names = Rf_allocVector(STRSXP, Rf_length(x)));
		SET_STRING_ELT(names, offset, newname);
		Rf_setAttrib(x, Symbols::NamesSymbol, names);
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
	    int *pindx = INTEGER0(indx);
	    names = Rf_getAttrib(x, Symbols::DimNamesSymbol);
	    for (i = 0; i < ndims; i++) {
		pindx[i] = int(
		    Rf_get1index(CAR(subs), VECTOR_ELT(names, i),
			      pdims[i],
			      /*partial ok*/FALSE, -1, call));
		subs = subs->tail();
		if (pindx[i] < 0 ||
		    pindx[i] >= pdims[i])
		    Rf_error(_("[[ ]] subscript (%d) out of bounds"), i+1);
	    }
	    offset = 0;
	    for (i = (ndims - 1); i > 0; i--)
		offset = (offset + pindx[i]) * pdims[i - 1];
	    offset += pindx[0];
	    SEXP slot = Rf_nthcdr(x, (int) offset);
	    SETCAR(slot, Rf_duplicate(y));
	    /* FIXME: add name */
	    UNPROTECT(1);
	}
	UNPROTECT(1);
    }
    else Rf_error(_("object of type '%s' is not subsettable"), Rf_type2char(TYPEOF(x)));

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
    SETTER_CLEAR_NAMED(xtop);
    if(S4) SET_S4_OBJECT(xtop);
    return xtop;
}

/* $<-(x, elt, val), and elt does not get evaluated it gets matched.
   to get DispatchOrEval to work we need to first translate it
   to a string
*/
HIDDEN SEXP do_subassign3(SEXP call, SEXP op, SEXP args, SEXP env)
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

    if (nlist == nullptr)
	nlist = Rf_installTrChar(STRING_ELT(arglist.get(1), 0));

    return R_subassign3_dflt(call, arglist.get(0), nlist, arglist.get(2));
}

/* used in "$<-" (above) and methods_list_dispatch.cpp */
SEXP R_subassign3_dflt(SEXP call, SEXP x, SEXP nlist, SEXP val)
{
    SEXP t;
    PROTECT_INDEX pvalidx, pxidx;
    Rboolean maybe_duplicate=FALSE;
    Rboolean S4; SEXP xS4 = nullptr;

    PROTECT_WITH_INDEX(x, &pxidx);
    PROTECT_WITH_INDEX(val, &pvalidx);
    S4 = Rboolean(IS_S4_OBJECT(x));

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
	if(x == nullptr)
	  Rf_errorcall(call, _("no method for assigning subsets of this S4 class"));
    }

    if ((Rf_isList(x) || Rf_isLanguage(x)) && !Rf_isNull(x)) {
	/* Here we do need to duplicate */
	if (maybe_duplicate)
	    REPROTECT(val = R_FixupRHS(x, val), pvalidx);
	if (TAG(x) == nlist) {
	    if (val == nullptr) {
		SET_ATTRIB(CDR(x), ATTRIB(x));
		IS_S4_OBJECT(x) ?  SET_S4_OBJECT(CDR(x)) : UNSET_S4_OBJECT(CDR(x));
		RAISE_NAMED(CDR(x), NAMED(x));
		x = CDR(x);
	    }
	    else
		SETCAR(x, val);
	}
	else {
	    for (t = x; t != nullptr; t = CDR(t))
		if (TAG(CDR(t)) == nlist) {
		    if (val == nullptr)
			SETCDR(t, CDDR(t));
		    else
			SETCAR(CDR(t), val);
		    break;
		}
		else if (CDR(t) == nullptr && val != nullptr) {
		    SETCDR(t, new PairList);
		    SET_TAG(CDR(t), nlist);
		    SETCADR(t, val);
		    break;
		}
	}
	if (x == nullptr && val != nullptr) {
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
	Rf_error(_("object of type '%s' is not subsettable"), Rf_type2char(TYPEOF(x)));
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
	names = Rf_getAttrib(x, Symbols::NamesSymbol);
	nx = Rf_xlength(x);
	nlist = PRINTNAME(nlist);
	if (Rf_isNull(val)) {
	    /* If "val" is NULL, this is an element deletion */
	    /* if there is a match to "nlist" otherwise "x" */
	    /* is unchanged.  The attributes need adjustment. */
	    if (names != nullptr) {
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
		    Rf_setAttrib(ans, Symbols::NamesSymbol, ansnames);
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
		    if (Rf_NonNullStringMatch(STRING_ELT(names, i), nlist)) {
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
		PROTECT(ans = Rf_allocVector(VECSXP, nx + 1));
		PROTECT(ansnames = Rf_allocVector(STRSXP, nx + 1));
		for (i = 0; i < nx; i++)
		    SET_VECTOR_ELT(ans, i, VECTOR_ELT(x, i));
		if (Rf_isNull(names)) {
		    for (i = 0; i < nx; i++)
			SET_STRING_ELT(ansnames, i, R_BlankString);
		}
		else {
		    for (i = 0; i < nx; i++)
			SET_STRING_ELT(ansnames, i, STRING_ELT(names, i));
		}
		SET_VECTOR_ELT(ans, nx, val);
		SET_STRING_ELT(ansnames, nx,  nlist);
		Rf_setAttrib(ans, Symbols::NamesSymbol, ansnames);
		Rf_copyMostAttrib(x, ans);
		UNPROTECT(2);
		x = ans;
	    }
	}
    }
    UNPROTECT(2);
    if(xS4 != nullptr)
	x = xS4; /* x was an env't, the data slot of xS4 */
    SETTER_CLEAR_NAMED(x);
    if(S4) SET_S4_OBJECT(x);
    return x;
}
