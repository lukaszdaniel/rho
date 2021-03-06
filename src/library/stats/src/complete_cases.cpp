/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-2013   The R Core Team
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

#include <Defn.h>
#define R_MSG_type	_("invalid 'type' (%s) of argument")

#include "localization.h"

/* Formerly in src/main/summary.cpp */

/* complete.cases(.) */
SEXP compcases(SEXP args)
{
    SEXP s, t, u, rval;
    int i, len;

    args = CDR(args);

    len = -1;

    for (s = args; s != nullptr; s = CDR(s)) {
	if (Rf_isList(CAR(s))) {
	    for (t = CAR(s); t != nullptr; t = CDR(t))
		if (Rf_isMatrix(CAR(t))) {
		    u = Rf_getAttrib(CAR(t), R_DimSymbol);
		    if (len < 0)
			len = INTEGER(u)[0];
		    else if (len != INTEGER(u)[0])
			goto bad;
		}
		else if (Rf_isVector(CAR(t))) {
		    if (len < 0)
			len = LENGTH(CAR(t));
		    else if (len != LENGTH(CAR(t)))
			goto bad;
		}
		else
		    Rf_error(R_MSG_type, Rf_type2char(TYPEOF(CAR(t))));
	}
	/* FIXME : Need to be careful with the use of Rf_isVector() */
	/* since this includes lists and expressions. */
	else if (Rf_isNewList(CAR(s))) {
	    int it, nt;
	    t = CAR(s);
	    nt = Rf_length(t);
	    /* 0-column data frames are a special case */
	    if(nt) {
		for (it = 0 ; it < nt ; it++) {
		    if (Rf_isMatrix(VECTOR_ELT(t, it))) {
			u = Rf_getAttrib(VECTOR_ELT(t, it), R_DimSymbol);
			if (len < 0)
			    len = INTEGER(u)[0];
			else if (len != INTEGER(u)[0])
			    goto bad;
		    }
		    else if (Rf_isVector(VECTOR_ELT(t, it))) {
			if (len < 0)
			    len = LENGTH(VECTOR_ELT(t, it));
			else if (len != LENGTH(VECTOR_ELT(t, it)))
			    goto bad;
		    }
		    else
			Rf_error(R_MSG_type, "unknown");
		}
	    } else {
		u = Rf_getAttrib(t, R_RowNamesSymbol);
		if (!Rf_isNull(u)) {
		    if (len < 0)
			len = LENGTH(u);
		    else if (len != INTEGER(u)[0])
			goto bad;
		}
	    }
	}
	else if (Rf_isMatrix(CAR(s))) {
	    u = Rf_getAttrib(CAR(s), R_DimSymbol);
	    if (len < 0)
		len = INTEGER(u)[0];
	    else if (len != INTEGER(u)[0])
		goto bad;
	}
	else if (Rf_isVector(CAR(s))) {
	    if (len < 0)
		len = LENGTH(CAR(s));
	    else if (len != LENGTH(CAR(s)))
		goto bad;
	}
	else
	    Rf_error(R_MSG_type, Rf_type2char(TYPEOF(CAR(s))));
    }

    if (len < 0)
	Rf_error(_("no input has determined the number of cases"));
    PROTECT(rval = Rf_allocVector(LGLSXP, len));
    for (i = 0; i < len; i++) INTEGER(rval)[i] = 1;
    /* FIXME : there is a lot of shared code here for vectors. */
    /* It should be abstracted out and optimized. */
    for (s = args; s != nullptr; s = CDR(s)) {
	if (Rf_isList(CAR(s))) {
	    /* Now we only need to worry about vectors */
	    /* since we use mod to handle arrays. */
	    /* FIXME : using mod like this causes */
	    /* a potential performance hit. */
	    for (t = CAR(s); t != nullptr; t = CDR(t)) {
		u = CAR(t);
		for (i = 0; i < LENGTH(u); i++) {
		    switch (TYPEOF(u)) {
		    case INTSXP:
		    case LGLSXP:
			if (INTEGER(u)[i] == NA_INTEGER)
			    INTEGER(rval)[i % len] = 0;
			break;
		    case REALSXP:
			if (ISNAN(REAL(u)[i]))
			    INTEGER(rval)[i % len] = 0;
			break;
		    case CPLXSXP:
			if (ISNAN(COMPLEX(u)[i].r) || ISNAN(COMPLEX(u)[i].i))
			    INTEGER(rval)[i % len] = 0;
			break;
		    case STRSXP:
			if (STRING_ELT(u, i) == NA_STRING)
			    INTEGER(rval)[i % len] = 0;
			break;
		    default:
			UNPROTECT(1);
			Rf_error(R_MSG_type, Rf_type2char(TYPEOF(u)));
		    }
		}
	    }
	}
	if (Rf_isNewList(CAR(s))) {
	    int it, nt;
	    t = CAR(s);
	    nt = Rf_length(t);
	    for (it = 0 ; it < nt ; it++) {
		u = VECTOR_ELT(t, it);
		for (i = 0; i < LENGTH(u); i++) {
		    switch (TYPEOF(u)) {
		    case INTSXP:
		    case LGLSXP:
			if (INTEGER(u)[i] == NA_INTEGER)
			    INTEGER(rval)[i % len] = 0;
			break;
		    case REALSXP:
			if (ISNAN(REAL(u)[i]))
			    INTEGER(rval)[i % len] = 0;
			break;
		    case CPLXSXP:
			if (ISNAN(COMPLEX(u)[i].r) || ISNAN(COMPLEX(u)[i].i))
			    INTEGER(rval)[i % len] = 0;
			break;
		    case STRSXP:
			if (STRING_ELT(u, i) == NA_STRING)
			    INTEGER(rval)[i % len] = 0;
			break;
		    default:
			UNPROTECT(1);
			Rf_error(R_MSG_type, Rf_type2char(TYPEOF(u)));
		    }
		}
	    }
	}
	else {
	    for (i = 0; i < LENGTH(CAR(s)); i++) {
		u = CAR(s);
		switch (TYPEOF(u)) {
		case INTSXP:
		case LGLSXP:
		    if (INTEGER(u)[i] == NA_INTEGER)
			INTEGER(rval)[i % len] = 0;
		    break;
		case REALSXP:
		    if (ISNAN(REAL(u)[i]))
			INTEGER(rval)[i % len] = 0;
		    break;
		case CPLXSXP:
		    if (ISNAN(COMPLEX(u)[i].r) || ISNAN(COMPLEX(u)[i].i))
			INTEGER(rval)[i % len] = 0;
		    break;
		case STRSXP:
		    if (STRING_ELT(u, i) == NA_STRING)
			INTEGER(rval)[i % len] = 0;
		    break;
		default:
		    UNPROTECT(1);
		    Rf_error(R_MSG_type, Rf_type2char(TYPEOF(u)));
		}
	    }
	}
    }
    UNPROTECT(1);
    return rval;

 bad:
    Rf_error(_("not all arguments have the same length"));
    return nullptr; /* -Wall */
}
