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
#include <Rmath.h>
#include <errno.h>
#include <utility>

#include "basedecl.h"

#include <rho/BinaryFunction.hpp>
#include <rho/BuiltInFunction.hpp>
#include <rho/GCStackRoot.hpp>
#include <rho/ComplexVector.hpp>
#include <rho/IntVector.hpp>
#include <rho/LogicalVector.hpp>
#include <rho/RawVector.hpp>
#include <rho/RealVector.hpp>

using namespace rho;
using namespace VectorOps;

/* interval at which to check interrupts, a guess */
// constexpr R_xlen_t NINTERRUPT = 10000000;

static SEXP string_relop(RELOP_TYPE code, SEXP s1, SEXP s2);

namespace {
    inline bool operator==(const Rcomplex& l, const Rcomplex& r)
    {
	return (l.r == r.r) && (l.i == r.i);
    }

    inline bool operator!=(const Rcomplex& l, const Rcomplex& r)
    {
	return !(l==r);
    }

    // bool isNaOrNaN(Logical value) { return isNA(value); }
    bool isNaOrNaN(int value)     { return isNA(value); }
    bool isNaOrNaN(double value)  { return std::isnan(value); }
    bool isNaOrNaN(Complex value) { return (isNaOrNaN(value.r)
					    || isNaOrNaN(value.i)); }

    template<typename T, typename Op>
    Logical withNaHandling(T lhs, T rhs, Op op) {
	bool eitherIsNaOrNan = isNaOrNaN(lhs) || isNaOrNaN(rhs);
	return eitherIsNaOrNan ? Logical::NA() : op(lhs, rhs);
    }

    template<typename T, typename Op>
    LogicalVector* relop_aux(const T* lhs, const T* rhs, Op op) {
	typedef typename T::value_type Value;
	return applyBinaryOperator(
	    [=](Value l, Value r) {
		return withNaHandling(l, r, op);
	    },
	    GeneralBinaryAttributeCopier(),
	    lhs, rhs);
    }

    template <class V>
    LogicalVector* relop(const V* vl, const V* vr, RELOP_TYPE code)
    {
	typedef typename V::value_type Value;

	switch (code) {
	case EQOP:
	    return relop_aux(vl, vr,
			     [](Value lhs, Value rhs) { return lhs == rhs; });
	case NEOP:
	    return relop_aux(vl, vr,
			     [](Value lhs, Value rhs) { return lhs != rhs; });
	case LTOP:
	    return relop_aux(vl, vr,
			     [](Value lhs, Value rhs) { return lhs < rhs; });
	case GTOP:
	    return relop_aux(vl, vr,
			     [](Value lhs, Value rhs) { return lhs > rhs; });
	case LEOP:
	    return relop_aux(vl, vr,
			     [](Value lhs, Value rhs) { return lhs <= rhs; });
	case GEOP:
	    return relop_aux(vl, vr,
			     [](Value lhs, Value rhs) { return lhs >= rhs; });
	}
	return nullptr;  // -Wall
    }

    template <class V>
    LogicalVector* relop_no_order(const V* vl, const V* vr, RELOP_TYPE code)
    {
	typedef typename V::value_type Value;

	switch (code) {
	case EQOP:
	    return relop_aux(vl, vr,
			     [](Value lhs, Value rhs) { return lhs == rhs; });
	case NEOP:
	    return relop_aux(vl, vr,
			     [](Value lhs, Value rhs) { return lhs != rhs; });
	default:
	    Rf_error(_("comparison of these types is not implemented"));
	}
	return nullptr;  // -Wall
    }
}  // anonymous namespace

SEXP do_relop(/*const*/ Expression* call,
			  const BuiltInFunction* op,
			  RObject* xarg, RObject* yarg)
{
    GCStackRoot<> x(xarg), y(yarg);

    R_xlen_t
	nx = Rf_xlength(x),
	ny = Rf_xlength(y);

    /* That symbols and calls were allowed was undocumented prior to
       R 2.5.0.  We deparse them as deparse() would, minus attributes */
    bool iS;
    if ((iS = Rf_isSymbol(x)) || TYPEOF(x) == LANGSXP) {
	x = Rf_ScalarString(
	    (iS) ? PRINTNAME(x) :
	    STRING_ELT(Rf_deparse1(x, FALSE, DEFAULTDEPARSE), 0));
    }
    if ((iS = Rf_isSymbol(y)) || TYPEOF(y) == LANGSXP) {
	y = Rf_ScalarString(
	    (iS) ? PRINTNAME(y) :
	    STRING_ELT(Rf_deparse1(y, FALSE, DEFAULTDEPARSE), 0));
    }

    if (Rf_isNull(x)) x = Rf_allocVector(INTSXP,0);
    if (Rf_isNull(y)) y = Rf_allocVector(INTSXP,0);
    if (!Rf_isVector(x) || !Rf_isVector(y))
	Rf_errorcall(call,
		  _("comparison (%d) is possible only for atomic and list types"),
		  op->variant());

    if (TYPEOF(x) == EXPRSXP || TYPEOF(y) == EXPRSXP) {
	Rf_errorcall(call, _("comparison is not allowed for expressions"));
    }

    /* ELSE :  x and y are both atomic or list */
	checkOperandsConformable(SEXP_downcast<VectorBase*>(xarg), SEXP_downcast<VectorBase*>(yarg));

  if (nx > 0 && ny > 0) {
    RELOP_TYPE opcode = RELOP_TYPE(op->variant());
    if (Rf_isString(x) || Rf_isString(y)) {
	// This case has not yet been brought into line with the
	// general rho pattern.
	VectorBase* xv = SEXP_downcast<VectorBase*>(Rf_coerceVector(x, STRSXP));
	VectorBase* yv = SEXP_downcast<VectorBase*>(Rf_coerceVector(y, STRSXP));
	if (((nx > ny) ? nx % ny : ny % nx) != 0) // mismatch
	    Rf_warningcall(call, _("longer object length is not a multiple of shorter object length"));
	GCStackRoot<VectorBase>
	    ans(SEXP_downcast<VectorBase*>(string_relop(opcode, xv, yv)));
	GeneralBinaryAttributeCopier::copyAttributes(ans, xv, yv);
	return ans;
    }
    else if (Rf_isComplex(x) || Rf_isComplex(y)) {
	GCStackRoot<ComplexVector>
	    vl(SEXP_downcast<ComplexVector*>(Rf_coerceVector(x, CPLXSXP)));
	GCStackRoot<ComplexVector>
	    vr(SEXP_downcast<ComplexVector*>(Rf_coerceVector(y, CPLXSXP)));
	return relop_no_order(vl.get(), vr.get(), opcode);
    }
    else if ((Rf_isNumeric(x) || Rf_isLogical(x)) && (Rf_isNumeric(y) || Rf_isLogical(y))) {
	GCStackRoot<RealVector>
	    vl(SEXP_downcast<RealVector*>(Rf_coerceVector(x, REALSXP)));
	GCStackRoot<RealVector>
	    vr(SEXP_downcast<RealVector*>(Rf_coerceVector(y, REALSXP)));
    return relop(vl.get(), vr.get(), opcode);
    } // rest of cases only apply when 'x' or 'y' is raw
    else if (Rf_isReal(x) || Rf_isReal(y)) {
	GCStackRoot<RealVector>
	    vl(SEXP_downcast<RealVector*>(Rf_coerceVector(x, REALSXP)));
	GCStackRoot<RealVector>
	    vr(SEXP_downcast<RealVector*>(Rf_coerceVector(y, REALSXP)));
	return relop(vl.get(), vr.get(), opcode);
    }
    else if (Rf_isInteger(x) || Rf_isInteger(y)) {
	GCStackRoot<IntVector>
	    vl(SEXP_downcast<IntVector*>(Rf_coerceVector(x, INTSXP)));
	GCStackRoot<IntVector>
	    vr(SEXP_downcast<IntVector*>(Rf_coerceVector(y, INTSXP)));
	return relop(vl.get(), vr.get(), opcode);
    }
    else if (Rf_isLogical(x) || Rf_isLogical(y)) {
	// TODO(kmillar): do this without promoting to integer.
	GCStackRoot<IntVector>
	    vl(SEXP_downcast<IntVector*>(Rf_coerceVector(x, INTSXP)));
	GCStackRoot<IntVector>
	    vr(SEXP_downcast<IntVector*>(Rf_coerceVector(y, INTSXP)));
	return relop(vl.get(), vr.get(), opcode);
    }
    else if (TYPEOF(x) == RAWSXP || TYPEOF(y) == RAWSXP) {
	GCStackRoot<RawVector>
	    vl(SEXP_downcast<RawVector*>(Rf_coerceVector(x, RAWSXP)));
	GCStackRoot<RawVector>
	    vr(SEXP_downcast<RawVector*>(Rf_coerceVector(y, RAWSXP)));
	return relop(vl.get(), vr.get(), opcode);
    } else Rf_errorcall(call, _("comparison of these types is not implemented"));
  }  else {
      GCStackRoot<> val(Rf_allocVector(LGLSXP, 0));
      GeneralBinaryAttributeCopier::copyAttributes(
	  SEXP_downcast<VectorBase*>(val.get()),
	  SEXP_downcast<VectorBase*>(xarg),
	  SEXP_downcast<VectorBase*>(yarg));
      return val;
  }
    return nullptr;  // -Wall
}

/* POSIX allows EINVAL when one of the strings contains characters
   outside the collation domain. */
static SEXP string_relop(RELOP_TYPE code, SEXP s1, SEXP s2)
{
    R_xlen_t i, n, n1, n2, res;
    SEXP ans, c1, c2;
    const void *vmax = vmaxget(); // for Scollate

    n1 = XLENGTH(s1);
    n2 = XLENGTH(s2);
    n = std::max(n1, n2);
    PROTECT(s1);
    PROTECT(s2);
    PROTECT(ans = Rf_allocVector(LGLSXP, n));
    int *pa = LOGICAL(ans);

    switch (code) {
    case EQOP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i % n1);
	    c2 = STRING_ELT(s2, i % n2);
	    if (c1 == R_NaString || c2 == R_NaString)
		pa[i] = R_NaLog;
	    else
		pa[i] = Rf_Seql(c1, c2) ? 1 : 0;
	}
	break;
    case NEOP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i % n1);
	    c2 = STRING_ELT(s2, i % n2);
	    if (c1 == R_NaString || c2 == R_NaString)
		pa[i] = R_NaLog;
	    else
		pa[i] = Rf_Seql(c1, c2) ? 0 : 1;
	}
	break;
    case LTOP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i % n1);
	    c2 = STRING_ELT(s2, i % n2);
	    if (c1 == R_NaString || c2 == R_NaString)
		pa[i] = R_NaLog;
	    else if (c1 == c2)
		pa[i] = 0;
	    else {
		errno = 0;
		res = Scollate(c1, c2);
		if(errno)
		    pa[i] = R_NaLog;
		else
		    pa[i] = (res < 0) ? 1 : 0;
	    }
	}
	break;
    case GTOP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i % n1);
	    c2 = STRING_ELT(s2, i % n2);
	    if (c1 == R_NaString || c2 == R_NaString)
		pa[i] = R_NaLog;
	    else if (c1 == c2)
		pa[i] = 0;
	    else {
		errno = 0;
		res = Scollate(c1, c2);
		if(errno)
		    pa[i] = R_NaLog;
		else
		    pa[i] = (res > 0) ? 1 : 0;
	    }
	}
	break;
    case LEOP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i % n1);
	    c2 = STRING_ELT(s2, i % n2);
	    if (c1 == R_NaString || c2 == R_NaString)
		pa[i] = R_NaLog;
	    else if (c1 == c2)
		pa[i] = 1;
	    else {
		errno = 0;
		res = Scollate(c1, c2);
		if(errno)
		    pa[i] = R_NaLog;
		else
		    pa[i] = (res <= 0) ? 1 : 0;
	    }
	}
	break;
    case GEOP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i % n1);
	    c2 = STRING_ELT(s2, i % n2);
	    if (c1 == R_NaString || c2 == R_NaString)
		pa[i] = R_NaLog;
	    else if (c1 == c2)
		pa[i] = 1;
	    else {
		errno = 0;
		res = Scollate(c1, c2);
		if(errno)
		    pa[i] = R_NaLog;
		else
		    pa[i] = (res >= 0) ? 1 : 0;
	    }
	}
	break;
    }
    UNPROTECT(3);
    vmaxset(vmax);
    return ans;
}



#define BIT(op, name)							\
    SEXP ans;								\
    int np = 0;								\
    if(Rf_isReal(a)) {a = PROTECT(Rf_coerceVector(a, INTSXP)); np++;}		\
    if(Rf_isReal(b)) {b = PROTECT(Rf_coerceVector(b, INTSXP)); np++;}		\
    if (TYPEOF(a) != TYPEOF(b))						\
	Rf_error(_("'a' and 'b' must have the same type"));		\
    switch(TYPEOF(a)) {							\
    case INTSXP:							\
	{								\
	    R_xlen_t i;						\
	    R_xlen_t m = XLENGTH(a), n = XLENGTH(b),			\
		mn = (m && n) ? std::max(m, n) : 0;			\
	    ans = Rf_allocVector(INTSXP, mn);				\
	    int *pans = INTEGER(ans);					\
	    const int *pa = INTEGER_RO(a), *pb = INTEGER_RO(b);		\
	for(i = 0; i < mn; i++) { \
	    int aa = pa[i%m], bb =  pb[i%n]; \
	    pans[i] = (aa == R_NaInt || bb == R_NaInt) ? R_NaInt : aa op bb; \
	} \
	}								\
	break;								\
    default:								\
	UNIMPLEMENTED_TYPE(name, a);					\
    }									\
    if(np) UNPROTECT(np);						\
    return ans

static SEXP bitwiseAnd(SEXP a, SEXP b)
{
    BIT(&, "bitwAnd");
}

static SEXP bitwiseOr(SEXP a, SEXP b)
{
    BIT(|, "bitwOr");
}

static SEXP bitwiseXor(SEXP a, SEXP b)
{
    BIT(^, "bitwXor");
}

static SEXP bitwiseShiftL(SEXP a, SEXP b)
{
    SEXP ans;
    int np = 0;
    if(Rf_isReal(a)) {a = PROTECT(Rf_coerceVector(a, INTSXP)); np++;}
    if(!Rf_isInteger(b)) {b = PROTECT(Rf_coerceVector(b, INTSXP)); np++;}
    if (TYPEOF(a) != TYPEOF(b))
	Rf_error(_("'a' and 'b' must have the same type"));

    switch(TYPEOF(a)) {
    case INTSXP:
	{
	    R_xlen_t i;
	    R_xlen_t m = XLENGTH(a), n = XLENGTH(b),
		mn = (m && n) ? std::max(m, n) : 0;
	    ans = Rf_allocVector(INTSXP, mn);
	    int *pans = INTEGER(ans);
	    const int *pa = INTEGER_RO(a), *pb = INTEGER_RO(b);
	for(i = 0; i < mn; i++) {
	    int aa = pa[i%m], bb = pb[i%n];
	    pans[i] = 
			(aa == R_NaInt || bb == R_NaInt ||
			 bb < 0 || bb > 31) ?
			R_NaInt : ((unsigned int)aa << bb);
	}
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("bitShiftL", a);
    }
    if(np) UNPROTECT(np);
    return ans;
}

static SEXP bitwiseShiftR(SEXP a, SEXP b)
{
    SEXP ans;
    int np = 0;
    if(Rf_isReal(a)) {a = PROTECT(Rf_coerceVector(a, INTSXP)); np++;}
    if(!Rf_isInteger(b)) {b = PROTECT(Rf_coerceVector(b, INTSXP)); np++;}
    if (TYPEOF(a) != TYPEOF(b))
	Rf_error(_("'a' and 'b' must have the same type"));

    switch(TYPEOF(a)) {
    case INTSXP:
	{
	    R_xlen_t i;
	    R_xlen_t m = XLENGTH(a), n = XLENGTH(b),
		mn = (m && n) ? std::max(m, n) : 0;
	    ans = Rf_allocVector(INTSXP, mn);
	    int *pans = INTEGER(ans);
	    const int *pa = INTEGER_RO(a), *pb = INTEGER_RO(b);
	for(i = 0; i < mn; i++) {
	    int aa = pa[i%m], bb = pb[i%n];
	    pans[i] = 
			(aa == R_NaInt || bb == R_NaInt ||
			 bb < 0 || bb > 31) ?
			R_NaInt : ((unsigned int)aa >> bb);
	}
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("bitShiftR", a);
    }
    if(np) UNPROTECT(np);
    return ans;
}

HIDDEN SEXP do_bitwise(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* y_)
{
    switch(op->variant()) {
    case 1:
	return bitwiseAnd(x_, y_);
    case 3:
	return bitwiseOr(x_, y_);
    case 4:
	return bitwiseXor(x_, y_);
    case 5:
	return bitwiseShiftL(x_, y_);
    case 6:
	return bitwiseShiftR(x_, y_);
    }
    return nullptr;  // unreachable.
}

HIDDEN SEXP do_bitwise_not(/*const*/ Expression* call, const BuiltInFunction* op, RObject* a) {
    int np = 0;
    if(Rf_isReal(a)) {a = PROTECT(Rf_coerceVector(a, INTSXP)); np++;}
    R_xlen_t i, m = XLENGTH(a);
    SEXP ans = Rf_allocVector(TYPEOF(a), m);
    switch(TYPEOF(a)) {
    case INTSXP:
	for(i = 0; i < m; i++) {
	    int aa = INTEGER(a)[i];
	    INTEGER(ans)[i] = (aa == R_NaInt) ? aa : ~aa;
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("bitwNot", a);
    }
    if(np) UNPROTECT(np);
    return ans;
}
