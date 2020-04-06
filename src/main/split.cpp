/* $Id$
 *
 * This file is part of Rho, a project to refactor the R interpreter
 * into C++.  It may consist in whole or in part of program code and
 * documentation taken from the R project itself, incorporated into
 * Rho (and possibly MODIFIED) under the terms of the GNU General Public
 * Licence.
 * 
 * Rho is Copyright (C) 2008-14 Andrew R. Runnalls, subject to such other
 * copyrights and copyright restrictions as may be stated below.
 * 
 * Rho is not part of the R project, and bugs and other issues should
 * not be reported via r-bugs or other R project channels; instead refer
 * to the Rho website.
 * */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2006-2016 The R Core Team
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
#include <R_ext/Itermacros.h>

SEXP attribute_hidden do_split(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* f_)
{
    SEXP x, f, counts, vec, nm, nmj;
    Rboolean have_names;

    x = x_;
    f = f_;
    if (!Rf_isVector(x))
	Rf_error(_("first argument must be a vector"));
    if (!Rf_isFactor(f))
	Rf_error(_("second argument must be a factor"));
    int nlevs = Rf_nlevels(f);
    R_xlen_t nfac = XLENGTH(f_);
    R_xlen_t nobs = XLENGTH(x_);
    if (nfac <= 0 && nobs > 0)
	Rf_error(_("group length is 0 but data length > 0"));
    if (nfac > 0 && (nobs % nfac) != 0)
	Rf_warning(_("data length is not a multiple of split variable"));
    nm = Rf_getAttrib(x, R_NamesSymbol);
    have_names = RHOCONSTRUCT(Rboolean, nm != nullptr);

#ifdef LONG_VECTOR_SUPPORT
    if (IS_LONG_VEC(x))
# define _L_INTSXP_ REALSXP
# define _L_INTEG_  REAL
# define _L_int_    R_xlen_t
# include "split-incl.cpp"

# undef _L_INTSXP_
# undef _L_INTEG_
# undef _L_int_
    else
#endif

# define _L_INTSXP_ INTSXP
# define _L_INTEG_  INTEGER
# define _L_int_    int
# include "split-incl.cpp"

# undef _L_INTSXP_
# undef _L_INTEG_
# undef _L_int_

    Rf_setAttrib(vec, R_NamesSymbol, Rf_getAttrib(f, R_LevelsSymbol));
    UNPROTECT(2);
    return vec;
}
