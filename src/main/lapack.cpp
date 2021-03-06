/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2012 The R Core Team
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
#include <Rdynpriv.h>
#include <Rmodules/Rlapack.h>
#include "basedecl.h"

static R_LapackRoutines *ptr;

static int initialized = 0;

static void La_Init(void)
{
    int res = R_moduleCdynload("lapack", 1, 1); // -> ../modules/lapack/Lapack.cpp
    initialized = -1;
    if(!res) return;
    if(!ptr->do_lapack)
	Rf_error(_("LAPACK routines cannot be accessed in module"));
    initialized = 1;
    return;
}

HIDDEN SEXP
do_lapack(SEXP call, SEXP op, SEXP args, SEXP env)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr->do_lapack)(call, op, args, env);
    else {
	Rf_error(_("LAPACK routines cannot be loaded"));
	return nullptr;
    }
}


R_LapackRoutines *
R_setLapackRoutines(R_LapackRoutines *routines)
{
    R_LapackRoutines *tmp;
    tmp = ptr;
    ptr = routines;
    return(tmp);
}
