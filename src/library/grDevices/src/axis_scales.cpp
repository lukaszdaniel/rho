/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2004-11   The R Core Team.
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

#include <R.h>
#include <Rinternals.h>
#include <R_ext/GraphicsEngine.h>

#include "grDevices.h"
#include "localization.h"

SEXP R_CreateAtVector(SEXP axp, SEXP usr, SEXP nint, SEXP is_log)
{
    int nint_v = Rf_asInteger(nint);
    Rboolean logflag = Rboolean(Rf_asLogical(is_log));

    PROTECT(axp = Rf_coerceVector(axp, REALSXP));
    PROTECT(usr = Rf_coerceVector(usr, REALSXP));
    if(LENGTH(axp) != 3) Rf_error(_("'%s' must be numeric of length %d"), "axp", 3);
    if(LENGTH(usr) != 2) Rf_error(_("'%s' must be numeric of length %d"), "usr", 2);

    SEXP res = CreateAtVector(REAL(axp), REAL(usr), nint_v, logflag);
    // -> ../../../main/plot.cpp
    UNPROTECT(2);
    return res;
}

SEXP R_GAxisPars(SEXP usr, SEXP is_log, SEXP nintLog)
{
    Rboolean logflag = Rboolean(Rf_asLogical(is_log));
    int n = Rf_asInteger(nintLog);// will be changed on output ..
    double min, max;
    const char *nms[] = {"axp", "n", ""};
    SEXP axp, ans;

    usr = Rf_coerceVector(usr, REALSXP);
    if(LENGTH(usr) != 2) Rf_error(_("'%s' must be numeric of length %d"), "usr", 2);
    min = REAL(usr)[0];
    max = REAL(usr)[1];

    GAxisPars(&min, &max, &n, logflag, 0);// axis = 0 :<==> do not warn.. [TODO!]
    // -> ../../../main/graphics.cpp

    PROTECT(ans = Rf_mkNamed(VECSXP, nms));
    SET_VECTOR_ELT(ans, 0, (axp = Rf_allocVector(REALSXP, 2)));// protected
    SET_VECTOR_ELT(ans, 1, Rf_ScalarInteger(n));
    REAL(axp)[0] = min;
    REAL(axp)[1] = max;

    UNPROTECT(1);
    return ans;
}
