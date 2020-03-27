/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2017   The R Core Team.
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
#include <Print.h>
#include <Rinternals.h>

#include "rho/ArgMatcher.hpp"
#include "rho/Evaluator.hpp"
#include "rho/StackChecker.hpp"

using namespace rho;

/* Interface to the (polymorphous!)  options(...)  command.
 *
 * We have two kind of options:
 *   1) those used exclusively from R code,
 *	typically initialized in Rprofile.

 *	Their names need not appear here, but may, when we want
 *	to make sure that they are assigned `valid' values only.
 *
 *   2) Those used (and sometimes set) from C code;
 *	Either accessing and/or setting a global C variable,
 *	or just accessed by e.g.  Rf_GetOption1(Rf_install("pager"))
 *
 * A (complete?!) list of these (2):
 *
 *	"prompt"
 *	"continue"
 *	"expressions"
 *	"width"
 *	"digits"
 *	"echo"
 *	"verbose"
 *	"keep.source"
 *	"keep.source.pkgs"
 *	"browserNLdisabled"

 *	"de.cellwidth"		../unix/X11/ & ../gnuwin32/dataentry.cpp
 *	"device"
 *	"pager"
 *	"paper.size"		./devPS.c

 *	"timeout"		./connections.cpp

 *	"check.bounds"
 *	"error"
 *	"error.messages"
 *	"show.error.messages"
 *	"warn"
 *	"warning.length"
 *	"warning.expression"
 *	"nwarnings"

 *	"matprod"

 *
 * S additionally/instead has (and one might think about some)
 * "free",	"keep"
 * "length",	"memory"
 * "object.size"
 * "reference", "show"
 * "scrap"
 */


static SEXP Options(void)
{
    static SEXP sOptions = NULL;
    if(!sOptions) sOptions = Rf_install(".Options");
    return sOptions;
}

static SEXP FindTaggedItem(SEXP lst, SEXP tag)
{
    for ( ; lst != R_NilValue ; lst = CDR(lst)) {
	if (TAG(lst) == tag)
	    return lst;
    }
    return R_NilValue;
}

static SEXP makeErrorCall(SEXP fun)
{
    SEXP call;
    PROTECT(call = new CachingExpression);
    SETCAR(call, fun);
    UNPROTECT(1);
    return call;
}

SEXP Rf_GetOption(SEXP tag, SEXP rho)
{
    return Rf_GetOption1(tag);
}


SEXP Rf_GetOption1(SEXP tag)
{
    SEXP opt = SYMVALUE(Options());
    if (!Rf_isList(opt)) Rf_error(_("corrupted options list"));
    opt = FindTaggedItem(opt, tag);
    return CAR(opt);
}

int Rf_GetOptionWidth(void)
{
    int w;
    w = Rf_asInteger(Rf_GetOption1(Rf_install("width")));
    if (w < R_MIN_WIDTH_OPT || w > R_MAX_WIDTH_OPT) {
	Rf_warning(_("invalid printing width, used 80"));
	return 80;
    }
    return w;
}

int Rf_GetOptionDigits(void)
{
    int d;
    d = Rf_asInteger(Rf_GetOption1(Rf_install("digits")));
    if (d < R_MIN_DIGITS_OPT || d > R_MAX_DIGITS_OPT) {
	Rf_warning(_("invalid printing digits, used 7"));
	return 7;
    }
    return d;
}

attribute_hidden
int Rf_GetOptionCutoff(void)
{
    int w;
    w = Rf_asInteger(Rf_GetOption1(Rf_install("deparse.cutoff")));
    if (w == NA_INTEGER || w <= 0) {
	Rf_warning(_("invalid 'deparse.cutoff', used 60"));
	w = 60;
    }
    return w;
}

attribute_hidden
Rboolean Rf_GetOptionDeviceAsk(void)
{
    int ask;
    ask = Rf_asLogical(Rf_GetOption1(Rf_install("device.ask.default")));
    if(ask == NA_LOGICAL) {
	Rf_warning(_("invalid value for \"device.ask.default\", using FALSE"));
	return FALSE;
    }
    return RHOCONSTRUCT(Rboolean, ask != 0);
}


/* Change the value of an option or add a new option or, */
/* if called with value R_NilValue, remove that option. */

static SEXP SetOption(SEXP tag, SEXP value)
{
    SEXP opt, old, t;
    PROTECT(value);
    t = opt = SYMVALUE(Options());
    if (!Rf_isList(opt))
	Rf_error(_("corrupted options list"));
    opt = FindTaggedItem(opt, tag);

    /* The option is being removed. */
    if (value == R_NilValue) {
	for ( ; t != R_NilValue ; t = CDR(t))
	    if (TAG(CDR(t)) == tag) {
		old = CAR(CDR(t));
		SETCDR(t, CDDR(t));
		UNPROTECT(1); /* value */
		return old;
	    }
	UNPROTECT(1); /* value */
	return R_NilValue;
    }
    /* If the option is new, a new slot */
    /* is added to the end of .Options */
    if (opt == R_NilValue) {
	while (CDR(t) != R_NilValue)
	    t = CDR(t);
	SETCDR(t, Rf_allocList(1));
	opt = CDR(t);
	SET_TAG(opt, tag);
    }
    old = CAR(opt);
    SETCAR(opt, value);
    UNPROTECT(1); /* value */
    return old;
}

/* Set the width of lines for printing i.e. like options(width=...) */
/* Returns the previous value for the options. */

int attribute_hidden R_SetOptionWidth(int w)
{
    SEXP t, v;
    if (w < R_MIN_WIDTH_OPT) w = R_MIN_WIDTH_OPT;
    if (w > R_MAX_WIDTH_OPT) w = R_MAX_WIDTH_OPT;
    PROTECT(t = Rf_install("width"));
    PROTECT(v = Rf_ScalarInteger(w));
    v = SetOption(t, v);
    UNPROTECT(2);
    return INTEGER(v)[0];
}

int attribute_hidden R_SetOptionWarn(int w)
{
    SEXP t, v;

    t = Rf_install("warn");
    PROTECT(v = Rf_ScalarInteger(w));
    v = SetOption(t, v);
    UNPROTECT(1);
    return INTEGER(v)[0];
}

/* Note that options are stored as a dotted pair list */
/* This is barely historical, but is also useful. */

void attribute_hidden Rf_InitOptions(void)
{
    SEXP val, v;
    const char *p = nullptr;

#ifdef HAVE_RL_COMPLETION_MATCHES
    PROTECT(v = val = Rf_allocList(18));
#else
    PROTECT(v = val = Rf_allocList(17));
#endif

    SET_TAG(v, Rf_install("prompt"));
    SETCAR(v, Rf_mkString("> "));
    v = CDR(v);

    SET_TAG(v, Rf_install("continue"));
    SETCAR(v, Rf_mkString("+ "));
    v = CDR(v);

    SET_TAG(v, Rf_install("expressions"));
    SETCAR(v, Rf_ScalarInteger(StackChecker::depthLimit()));
    v = CDR(v);

    SET_TAG(v, Rf_install("width"));
    SETCAR(v, Rf_ScalarInteger(80));
    v = CDR(v);

    SET_TAG(v, Rf_install("deparse.cutoff"));
    SETCAR(v, Rf_ScalarInteger(60));
    v = CDR(v);

    SET_TAG(v, Rf_install("digits"));
    SETCAR(v, Rf_ScalarInteger(7));
    v = CDR(v);

    SET_TAG(v, Rf_install("echo"));
    SETCAR(v, Rf_ScalarLogical(!R_Slave));
    v = CDR(v);

    SET_TAG(v, Rf_install("verbose"));
    SETCAR(v, Rf_ScalarLogical(R_Verbose));
    v = CDR(v);

    SET_TAG(v, Rf_install("check.bounds"));
    SETCAR(v, Rf_ScalarLogical(0));	/* no checking */
    v = CDR(v);

    p = getenv("R_KEEP_PKG_SOURCE");
    R_KeepSource = (p && (strcmp(p, "yes") == 0)) ? RHO_TRUE : RHO_FALSE;

    SET_TAG(v, Rf_install("keep.source")); /* overridden in common.R */
    SETCAR(v, Rf_ScalarLogical(R_KeepSource));
    v = CDR(v);

    SET_TAG(v, Rf_install("keep.source.pkgs"));
    SETCAR(v, Rf_ScalarLogical(R_KeepSource));
    v = CDR(v);

    SET_TAG(v, Rf_install("warning.length"));
    SETCAR(v, Rf_ScalarInteger(1000));
    v = CDR(v);

    SET_TAG(v, Rf_install("nwarnings"));
    SETCAR(v, Rf_ScalarInteger(50));
    v = CDR(v);

    SET_TAG(v, Rf_install("OutDec"));
    SETCAR(v, Rf_mkString("."));
    v = CDR(v);

    SET_TAG(v, Rf_install("browserNLdisabled"));
    SETCAR(v, Rf_ScalarLogical(FALSE));
    v = CDR(v);

    p = getenv("R_C_BOUNDS_CHECK");
    R_CBoundsCheck = RHOCONSTRUCT(Rboolean, (p && (strcmp(p, "yes") == 0)) ? 1 : 0);

    SET_TAG(v, Rf_install("CBoundsCheck"));
    SETCAR(v, Rf_ScalarLogical(R_CBoundsCheck));
    v = CDR(v);

    SET_TAG(v, Rf_install("matprod"));
    switch(R_Matprod) {
	case MATPROD_DEFAULT: p = "default"; break;
	case MATPROD_INTERNAL: p = "internal"; break;
	case MATPROD_BLAS: p = "blas"; break;
	case MATPROD_DEFAULT_SIMD: p = "default.simd"; break;
    }
    SETCAR(v, Rf_mkString(p));
    v = CDR(v);

#ifdef HAVE_RL_COMPLETION_MATCHES
    /* value from Rf_initialize_R */
    SET_TAG(v, Rf_install("rl_word_breaks"));
    SETCAR(v, Rf_mkString(" \t\n\"\\'`><=%;,|&{()}"));
    set_rl_word_breaks(" \t\n\"\\'`><=%;,|&{()}");
#endif

    SET_SYMVALUE(Rf_install(".Options"), val);
    UNPROTECT(1);
}


SEXP attribute_hidden do_getOption(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x = CAR(args);
    if (!Rf_isString(x) || LENGTH(x) != 1)
	Rf_error(_("'%s' must be a character string"), "x");
    return Rf_duplicate(Rf_GetOption1(Rf_install(R_CHAR(STRING_ELT(x, 0)))));
}


/* This needs to manage R_Visible */
SEXP attribute_hidden do_options(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP names, value, options;

    /* Locate the options values in the symbol table.
       This will need to change if options are to live in the session
       frame.
       */

    options = SYMVALUE(Options());

    /* This code is not re-entrant and people have used it in
       finalizers.

       If a re-entrancy lock needs to be added, note that it
       would apply to R_SetOption* too.
    */

    checkArity(op, args);
    if (args == R_NilValue) {
	/* This is the zero argument case.
	   We alloc up a vector list and write the system values into it.
	*/
	int n = Rf_length(options);
	PROTECT(value = Rf_allocVector(VECSXP, n));
	PROTECT(names = Rf_allocVector(STRSXP, n));
	for (int i = 0; i < n; i++) {
	    SET_STRING_ELT(names, i, PRINTNAME(TAG(options)));
	    SET_VECTOR_ELT(value, i, Rf_duplicate(CAR(options)));
	    options = CDR(options);
	}
	SEXP sind = PROTECT(Rf_allocVector(INTSXP, n));
	int *indx = INTEGER(sind);
	for (int i = 0; i < n; i++) indx[i] = i;
	orderVector1(indx, n, names, TRUE, FALSE, R_NilValue);
	SEXP value2 = PROTECT(Rf_allocVector(VECSXP, n));
	SEXP names2 = PROTECT(Rf_allocVector(STRSXP, n));
	for(int i = 0; i < n; i++) {
	    SET_STRING_ELT(names2, i, STRING_ELT(names, indx[i]));
	    SET_VECTOR_ELT(value2, i, VECTOR_ELT(value, indx[i]));
	}
	Rf_setAttrib(value2, R_NamesSymbol, names2);
	UNPROTECT(5);
	R_Visible = TRUE;
	return value2;
    }

    /* The arguments to "options" can either be a sequence of
       name = value form, or can be a single list.
       This means that we must code so that both forms will work.
       [ Vomits quietly onto shoes ... ]
       */

    int n = Rf_length(args);
    if (n == 1 && (Rf_isPairList(CAR(args)) || Rf_isVectorList(CAR(args)))
	&& TAG(args) == R_NilValue ) {
	args = CAR(args);
	n = Rf_length(args);
    }
    PROTECT(value = Rf_allocVector(VECSXP, n));
    PROTECT(names = Rf_allocVector(STRSXP, n));

    SEXP argnames = R_NilValue;
    switch (TYPEOF(args)) {
    case NILSXP:
    case LISTSXP:
	break;
    case VECSXP:
	if(n > 0) {
	    argnames = Rf_getAttrib(args, R_NamesSymbol);
	    if(LENGTH(argnames) != n)
		Rf_error(_("list argument has no valid names"));
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("options", args);
    }

    R_Visible = FALSE;
    for (int i = 0 ; i < n ; i++) { /* i-th argument */
	SEXP argi = R_NilValue, namei = R_NilValue;
	switch (TYPEOF(args)) {
	case LISTSXP:
	    argi = CAR(args);
	    namei = Rf_EnsureString(TAG(args)); /* gives "" for no tag */
	    args = CDR(args);
	    break;
	case VECSXP:
	    argi = VECTOR_ELT(args, i);
	    namei = STRING_ELT(argnames, i);
	    break;
	default: /* already checked, but be safe here */
	    UNIMPLEMENTED_TYPE("options", args);
	}

	if (*R_CHAR(namei)) { /* name = value  ---> assignment */
	    SEXP tag = Rf_installTrChar(namei);
	    if (streql(R_CHAR(namei), "width")) {
		int k = Rf_asInteger(argi);
		if (k < R_MIN_WIDTH_OPT || k > R_MAX_WIDTH_OPT)
		    Rf_error(_("invalid 'width' parameter, allowed %d...%d"),
			  R_MIN_WIDTH_OPT, R_MAX_WIDTH_OPT);
		SET_VECTOR_ELT(value, i, SetOption(tag, Rf_ScalarInteger(k)));
	    }
	    else if (streql(R_CHAR(namei), "deparse.cutoff")) {
		int k = Rf_asInteger(argi);
		SET_VECTOR_ELT(value, i, SetOption(tag, Rf_ScalarInteger(k)));
	    }
	    else if (streql(R_CHAR(namei), "digits")) {
		int k = Rf_asInteger(argi);
		if (k < R_MIN_DIGITS_OPT || k > R_MAX_DIGITS_OPT)
		    Rf_error(_("invalid 'digits' parameter, allowed %d...%d"),
			  R_MIN_DIGITS_OPT, R_MAX_DIGITS_OPT);
		SET_VECTOR_ELT(value, i, SetOption(tag, Rf_ScalarInteger(k)));
	    }
	    else if (streql(R_CHAR(namei), "expressions")) {
		int k = Rf_asInteger(argi);
		StackChecker::setDepthLimit(k);
		SET_VECTOR_ELT(value, i, SetOption(tag, Rf_ScalarInteger(k)));
	    }
	    else if (streql(R_CHAR(namei), "keep.source")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    Rf_error(_("invalid value for '%s'"), R_CHAR(namei));
		int k = Rf_asLogical(argi);
		R_KeepSource = RHOCONSTRUCT(Rboolean, k);
		SET_VECTOR_ELT(value, i, SetOption(tag, Rf_ScalarLogical(k)));
	    }
	    else if (streql(R_CHAR(namei), "editor") && Rf_isString(argi)) {
		SEXP s =  Rf_asChar(argi);
		if (s == NA_STRING || LENGTH(s) == 0)
		    Rf_error(_("invalid value for '%s'"), R_CHAR(namei));
		SET_VECTOR_ELT(value, i, SetOption(tag, Rf_ScalarString(s)));
	    }
	    else if (streql(R_CHAR(namei), "continue")) {
		SEXP s =  Rf_asChar(argi);
		if (s == NA_STRING || LENGTH(s) == 0)
		    Rf_error(_("invalid value for '%s'"), R_CHAR(namei));
		/* We want to make sure these are in the native encoding */
		SET_VECTOR_ELT(value, i,
			       SetOption(tag, Rf_mkString(Rf_translateChar(s))));
	    }
	    else if (streql(R_CHAR(namei), "prompt")) {
		SEXP s =  Rf_asChar(argi);
		if (s == NA_STRING || LENGTH(s) == 0)
		    Rf_error(_("invalid value for '%s'"), R_CHAR(namei));
		/* We want to make sure these are in the native encoding */
		SET_VECTOR_ELT(value, i,
			       SetOption(tag, Rf_mkString(Rf_translateChar(s))));
	    }
	    else if (streql(R_CHAR(namei), "contrasts")) {
		if (TYPEOF(argi) != STRSXP || LENGTH(argi) != 2)
		    Rf_error(_("invalid value for '%s'"), R_CHAR(namei));
		SET_VECTOR_ELT(value, i, SetOption(tag, argi));
	    }
	    else if (streql(R_CHAR(namei), "check.bounds")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    Rf_error(_("invalid value for '%s'"), R_CHAR(namei));
		int k = Rf_asLogical(argi);
		/* R_CheckBounds = k; */
		SET_VECTOR_ELT(value, i, SetOption(tag, Rf_ScalarLogical(k)));
	    }
	    else if (streql(R_CHAR(namei), "warn")) {
		if (!Rf_isNumeric(argi) || LENGTH(argi) != 1)
		    Rf_error(_("invalid value for '%s'"), R_CHAR(namei));
		SET_VECTOR_ELT(value, i, SetOption(tag, argi));
	    }
	    else if (streql(R_CHAR(namei), "warning.length")) {
		int k = Rf_asInteger(argi);
		if (k < 100 || k > 8170)
		    Rf_error(_("invalid value for '%s'"), R_CHAR(namei));
		R_WarnLength = k;
		SET_VECTOR_ELT(value, i, SetOption(tag, argi));
	    }
	    else if (streql(R_CHAR(namei), "warning.expression"))  {
		if( !Rf_isLanguage(argi) &&  !Rf_isExpression(argi) )
		    Rf_error(_("invalid value for '%s'"), R_CHAR(namei));
		SET_VECTOR_ELT(value, i, SetOption(tag, argi));
	    }
	    else if (streql(R_CHAR(namei), "max.print")) {
		int k = Rf_asInteger(argi);
		if (k < 1) Rf_error(_("invalid value for '%s'"), R_CHAR(namei));
		SET_VECTOR_ELT(value, i, SetOption(tag, Rf_ScalarInteger(k)));
	    }
	    else if (streql(R_CHAR(namei), "nwarnings")) {
		int k = Rf_asInteger(argi);
		if (k < 1) Rf_error(_("invalid value for '%s'"), R_CHAR(namei));
		R_nwarnings = k;
		R_CollectWarnings = 0; /* force a reset */
		SET_VECTOR_ELT(value, i, SetOption(tag, Rf_ScalarInteger(k)));
	    }
	    else if (streql(R_CHAR(namei), "error")) {
		if(Rf_isFunction(argi))
		  argi = makeErrorCall(argi);
		else if( !Rf_isLanguage(argi) && !Rf_isExpression(argi) )
		    Rf_error(_("invalid value for '%s'"), R_CHAR(namei));
		SET_VECTOR_ELT(value, i, SetOption(tag, argi));
	    }
/* handle this here to avoid GetOption during error handling */
	    else if (streql(R_CHAR(namei), "show.error.messages")) {
		if( !Rf_isLogical(argi) && LENGTH(argi) != 1 )
		    Rf_error(_("invalid value for '%s'"), R_CHAR(namei));
		SET_VECTOR_ELT(value, i, SetOption(tag, argi));
		R_ShowErrorMessages = LOGICAL(argi)[0];
	    }
	    else if (streql(R_CHAR(namei), "echo")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    Rf_error(_("invalid value for '%s'"), R_CHAR(namei));
		int k = Rf_asLogical(argi);
		/* Should be quicker than checking options(echo)
		   every time R prompts for input:
		   */
		R_Slave = RHOCONSTRUCT(Rboolean, !k);
		SET_VECTOR_ELT(value, i, SetOption(tag, Rf_ScalarLogical(k)));
	    }
	    else if (streql(R_CHAR(namei), "OutDec")) {
		if (TYPEOF(argi) != STRSXP || LENGTH(argi) != 1)
		    Rf_error(_("invalid value for '%s'"), R_CHAR(namei));
		static char sdec[11];
		if(R_nchar(STRING_ELT(argi, 0), Chars,
			   /* allowNA = */ FALSE, /* keepNA = */ FALSE,
			   "OutDec") != 1) // will become an error
		    Rf_warning(_("'OutDec' must be a string of one character"));
		strncpy(sdec, R_CHAR(STRING_ELT(argi, 0)), 10);
		sdec[10] = '\0';
		OutDec = sdec;
		SET_VECTOR_ELT(value, i, SetOption(tag, Rf_duplicate(argi)));
	    }
	    else if (streql(R_CHAR(namei), "max.contour.segments")) {
		int k = Rf_asInteger(argi);
		if (k < 0) // also many times above: rely on  NA_INTEGER  <  <finite_int>
		    Rf_error(_("invalid value for '%s'"), R_CHAR(namei));
		max_contour_segments = k;
		SET_VECTOR_ELT(value, i, SetOption(tag, Rf_ScalarInteger(k)));
	    }
	    else if (streql(R_CHAR(namei), "rl_word_breaks")) {
		if (TYPEOF(argi) != STRSXP || LENGTH(argi) != 1)
		    Rf_error(_("invalid value for '%s'"), R_CHAR(namei));
#ifdef HAVE_RL_COMPLETION_MATCHES
		set_rl_word_breaks(R_CHAR(STRING_ELT(argi, 0)));
#endif
		SET_VECTOR_ELT(value, i, SetOption(tag, Rf_duplicate(argi)));
	    }
	    else if (streql(R_CHAR(namei), "warnPartialMatchDollar")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    Rf_error(_("invalid value for '%s'"), R_CHAR(namei));
		int k = Rf_asLogical(argi);
		R_warn_partial_match_dollar = RHOCONSTRUCT(Rboolean, k);
		SET_VECTOR_ELT(value, i, SetOption(tag, Rf_ScalarLogical(k)));
	    }
	    else if (streql(R_CHAR(namei), "warnPartialMatchArgs")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    Rf_error(_("invalid value for '%s'"), R_CHAR(namei));
		int k = Rf_asLogical(argi);
		ArgMatcher::enableWarnOnPartialMatch(k);
		SET_VECTOR_ELT(value, i, SetOption(tag, Rf_ScalarLogical(k)));
	    }
	    else if (streql(R_CHAR(namei), "warnPartialMatchAttr")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    Rf_error(_("invalid value for '%s'"), R_CHAR(namei));
		int k = Rf_asLogical(argi);
		R_warn_partial_match_attr = RHOCONSTRUCT(Rboolean, k);
		SET_VECTOR_ELT(value, i, SetOption(tag, Rf_ScalarLogical(k)));
	    }
	    else if (streql(R_CHAR(namei), "showWarnCalls")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    Rf_error(_("invalid value for '%s'"), R_CHAR(namei));
		int k = Rf_asLogical(argi);
		R_ShowWarnCalls = RHOCONSTRUCT(Rboolean, k);
		SET_VECTOR_ELT(value, i, SetOption(tag, Rf_ScalarLogical(k)));
	    }
	    else if (streql(R_CHAR(namei), "showErrorCalls")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    Rf_error(_("invalid value for '%s'"), R_CHAR(namei));
		int k = Rf_asLogical(argi);
		R_ShowErrorCalls = RHOCONSTRUCT(Rboolean, k);
		SET_VECTOR_ELT(value, i, SetOption(tag, Rf_ScalarLogical(k)));
	    }
	    else if (streql(R_CHAR(namei), "showNCalls")) {
		int k = Rf_asInteger(argi);
		if (k < 30 || k > 500 || k == NA_INTEGER || LENGTH(argi) != 1)
		    Rf_error(_("invalid value for '%s'"), R_CHAR(namei));
		R_NShowCalls = k;
		SET_VECTOR_ELT(value, i, SetOption(tag, Rf_ScalarInteger(k)));
	    }
	    else if (streql(R_CHAR(namei), "par.ask.default")) {
		Rf_error(_("\"par.ask.default\" has been replaced by \"device.ask.default\""));
	    }
	    else if (streql(R_CHAR(namei), "browserNLdisabled")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    Rf_error(_("invalid value for '%s'"), R_CHAR(namei));
		int k = Rf_asLogical(argi);
		if (k == NA_LOGICAL)
		    Rf_error(_("invalid value for '%s'"), R_CHAR(namei));
		R_DisableNLinBrowser = RHOCONSTRUCT(Rboolean, k);
		SET_VECTOR_ELT(value, i, SetOption(tag, Rf_ScalarLogical(k)));
	    }
	    else if (streql(R_CHAR(namei), "CBoundsCheck")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    Rf_error(_("invalid value for '%s'"), R_CHAR(namei));
		int k = Rf_asLogical(argi);
		R_CBoundsCheck = RHOCONSTRUCT(Rboolean, k);
		SET_VECTOR_ELT(value, i, SetOption(tag, Rf_ScalarLogical(k)));
	    }
	    else if (streql(CHAR(namei), "matprod")) {
		SEXP s = Rf_asChar(argi);
		if (s == NA_STRING || LENGTH(s) == 0)
		    Rf_error(_("invalid value for '%s'"), CHAR(namei));
		if (streql(CHAR(s), "default"))
		    R_Matprod = MATPROD_DEFAULT;
		else if (streql(CHAR(s), "internal"))
		    R_Matprod = MATPROD_INTERNAL;
		else if (streql(CHAR(s), "blas"))
		    R_Matprod = MATPROD_BLAS;
		else if (streql(CHAR(s), "default.simd"))
		    R_Matprod = MATPROD_DEFAULT_SIMD;
		else
		    Rf_error(_("invalid value for '%s'"), CHAR(namei));
		SET_VECTOR_ELT(value, i, SetOption(tag, Rf_duplicate(argi)));
	    }
	    else {
		SET_VECTOR_ELT(value, i, SetOption(tag, Rf_duplicate(argi)));
	    }
	    SET_STRING_ELT(names, i, namei);
	}
	else { /* querying arg */
	    const char *tag;
	    if (!Rf_isString(argi) || LENGTH(argi) <= 0)
		Rf_error(_("invalid argument"));
	    tag = R_CHAR(STRING_ELT(argi, 0));
	    if (streql(tag, "par.ask.default")) {
		Rf_error(_("\"par.ask.default\" has been replaced by \"device.ask.default\""));
	    }

	    SET_VECTOR_ELT(value, i, Rf_duplicate(CAR(FindTaggedItem(options, Rf_install(tag)))));
	    SET_STRING_ELT(names, i, STRING_ELT(argi, 0));
	    R_Visible = TRUE;
	}
    } /* for() */
    Rf_setAttrib(value, R_NamesSymbol, names);
    UNPROTECT(2);
    return value;
}
