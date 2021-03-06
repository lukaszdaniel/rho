/*
 *  R : A Computer Language for Statistical Data Analysis
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

/** @file ExpressionVector.cpp
 *
 * Implementation of class ExpressionVector and related functions.
 */

#include <rho/ExpressionVector.hpp>

using namespace rho;

namespace rho
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &isExpressionptr = Rf_isExpression;
        const auto &XVECTOR_ELTp = XVECTOR_ELT;
    } // namespace ForceNonInline

    template <>
    const char *ExpressionVector::staticTypeName()
    {
        return "expression";
    }
} // namespace rho

// ***** C interface *****

Rboolean Rf_isExpression(SEXP s)
{
    return Rboolean(s && TYPEOF(s) == EXPRSXP);
}

SEXP XVECTOR_ELT(SEXP x, R_xlen_t i)
{
    using namespace rho;
    if (x && x->sexptype() == EXPRSXP)
    {
        ExpressionVector *ev = SEXP_downcast<ExpressionVector *>(x, false);
        return (*ev)[VectorBase::size_type(i)];
    }
    else
    {
        Rf_error("'%s' function can only be applied to an expression vector, not a '%s'",
                 "XVECTOR_ELT()", Rf_type2char(TYPEOF(x)));
    }
}

SEXP SET_XVECTOR_ELT(SEXP x, R_xlen_t i, SEXP v)
{
    ExpressionVector *ev = SEXP_downcast<ExpressionVector *>(x, false);
    (*ev)[i] = v;
    return v;
}
