/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-2017	The R Core Team.
 *  Copyright (C) 1995-1998	Robert Gentleman and Ross Ihaka.
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
 *
 *  print.default()  ->	 do_printdefault (with call tree below)
 *
 *  auto-printing   ->  PrintValueEnv
 *                      -> PrintValueRec
 *                      -> call print() for objects
 *  Note that auto-printing does not call print.default.
 *  PrintValue, R_PV are similar to auto-printing.
 *
 *  do_printdefault
 *	-> PrintDefaults
 *	-> CustomPrintValue
 *	    -> PrintValueRec
 *		-> __ITSELF__  (recursion)
 *		-> PrintGenericVector	-> PrintValueRec  (recursion)
 *		-> printList		-> PrintValueRec  (recursion)
 *		-> printAttributes	-> PrintValueRec  (recursion)
 *		-> PrintSpecial
 *		-> PrintExpression
 *		-> PrintLanguage        -> PrintLanguageEtc
 *		-> PrintClosure         -> PrintLanguageEtc
 *		-> printVector		>>>>> ./printvector.cpp
 *		-> printNamedVector	>>>>> ./printvector.cpp
 *		-> printMatrix		>>>>> ./printarray.cpp
 *		-> printArray		>>>>> ./printarray.cpp
 *
 *  do_prmatrix
 *	-> PrintDefaults
 *	-> printMatrix			>>>>> ./printarray.cpp
 *
 *
 *  See ./printutils.cpp	 for general remarks on Printing
 *			 and the Rf_EncodeString() and all Encode*() utils,
 *
 *  Also ./printvector.cpp,  ./printarray.cpp
 *
 *  do_sink moved to connections.cpp as of 1.3.0
 *
 *  <FIXME> These routines are not re-entrant: they reset the
 *  global R_print.
 *  </FIXME>
 */

#define R_NO_REMAP

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Localization.h>
#include <Internal.h>
#include "Print.h"
#include "Fileio.h"
#include "Rconnections.h"
#include <R_ext/RS.h>
#include "rho/GCStackRoot.hpp"

using namespace rho;

/* Global print parameter struct: */
R_print_par_t R_print;

static void printAttributes(SEXP, SEXP, Rboolean);
static void PrintSpecial(SEXP);
static void PrintLanguageEtc(SEXP, Rboolean, Rboolean);


#define TAGBUFLEN 256
#define TAGBUFLEN0 TAGBUFLEN + 6
static char tagbuf[TAGBUFLEN0 * 2]; /* over-allocate to allow overflow check */

RObject* getNaStringNoQuote() {
    static GCRoot<> na_string_noquote(Rf_mkChar("<NA>"));
    return na_string_noquote;
}

/* Used in X11 module for dataentry */
/* NB this is called by R.app even though it is in no public header, so
   alter there if you alter this */
void Rf_PrintDefaults(void)
{
    R_print.na_string = NA_STRING;
    R_print.na_string_noquote = getNaStringNoQuote();
    R_print.na_width = int(strlen(R_CHAR(R_print.na_string)));
    R_print.na_width_noquote = int(strlen(R_CHAR(R_print.na_string_noquote)));
    R_print.quote = 1;
    R_print.right = Rprt_adj_left;
    R_print.digits = Rf_GetOptionDigits();
    R_print.scipen = Rf_asInteger(Rf_GetOption1(Rf_install("scipen")));
    if (R_print.scipen == NA_INTEGER) R_print.scipen = 0;
    R_print.max = Rf_asInteger(Rf_GetOption1(Rf_install("max.print")));
    if (R_print.max == NA_INTEGER || R_print.max < 0) R_print.max = 99999;
    else if(R_print.max == INT_MAX) R_print.max--; // so we can add
    R_print.gap = 1;
    R_print.width = Rf_GetOptionWidth();
    R_print.useSource = USESOURCE;
    R_print.cutoff = Rf_GetOptionCutoff();
}

SEXP attribute_hidden do_invisible(/*const*/ Expression* call, const BuiltInFunction* op, int num_args, ...)
{
    switch (num_args) {
    case 0:
	return R_NilValue;
    case 1:
    {
	call->check1arg("x");
	UNPACK_VA_ARGS(num_args, (x));
	return x;
    }
    default:
	op->checkNumArgs(num_args, 1, call);
	return call;/* never used, just for -Wall */
    }
}

/* This is *only* called via outdated R_level prmatrix() : */
SEXP attribute_hidden do_prmatrix(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* rowlab_, RObject* collab_, RObject* quote_, RObject* right_, RObject* na_print_)
{
    int quote;
    SEXP x, rowlab, collab, naprint;
    char *rowname = nullptr, *colname = nullptr;

    Rf_PrintDefaults();
    x = x_;
    rowlab = rowlab_;
    collab = collab_;

    quote = Rf_asInteger(quote_);
    R_print.right = Rprt_adj(Rf_asInteger(right_));
    naprint = na_print_;
    if(!Rf_isNull(naprint))  {
	if(!Rf_isString(naprint) || LENGTH(naprint) < 1)
	    Rf_error(_("invalid 'na.print' specification"));
	R_print.na_string = R_print.na_string_noquote = STRING_ELT(naprint, 0);
	R_print.na_width = R_print.na_width_noquote =
	    int(strlen(R_CHAR(R_print.na_string)));
    }

    if (Rf_length(rowlab) == 0) rowlab = R_NilValue;
    if (Rf_length(collab) == 0) collab = R_NilValue;
    if (!Rf_isNull(rowlab) && !Rf_isString(rowlab))
	Rf_error(_("invalid row labels"));
    if (!Rf_isNull(collab) && !Rf_isString(collab))
	Rf_error(_("invalid column labels"));

    printMatrix(x, 0, Rf_getAttrib(x, R_DimSymbol), quote, R_print.right,
		rowlab, collab, rowname, colname);
    Rf_PrintDefaults(); /* reset, as na.print.etc may have been set */
    return x;
}/* do_prmatrix */

/* .Internal( print.function(f, useSource, ...)) */
SEXP attribute_hidden do_printfunction(/*const*/ Expression* call, const BuiltInFunction* op, Environment* rho, RObject* const* args, int num_args, const PairList* tags)
{
    op->checkNumArgs(num_args, call);
    SEXP s = args[0];
    switch (TYPEOF(s)) {
    case CLOSXP:
	PrintLanguageEtc(s, Rboolean(Rf_asLogical(args[1])), /*is closure = */ TRUE);
	printAttributes(s, rho, FALSE);
	break;
    case BUILTINSXP:
    case SPECIALSXP:
	PrintSpecial(s);
	break;

    default: /* if(!Rf_isFunction(s)) */
	Rf_error(_("non-function argument to .Internal(print.function(.))"));
    }
    return s;
}

/* PrintLanguage() or PrintClosure() : */
static void PrintLanguageEtc(SEXP s, Rboolean useSource, Rboolean isClosure)
{
    int i;
    SEXP t = Rf_getAttrib(s, R_SrcrefSymbol);
    Rboolean useSrc = Rboolean(useSource && Rf_isInteger(t));
    if (useSrc) {
	PROTECT(t = Rf_lang2(R_AsCharacterSymbol, t));
	t = Rf_eval(t, R_BaseEnv);
	UNPROTECT(1);
    } else {
	t = Rf_deparse1w(s, Rboolean(0), useSource | DEFAULTDEPARSE);
    }
    PROTECT(t);
    for (i = 0; i < LENGTH(t); i++) {
 	Rprintf("%s\n", Rf_translateChar(STRING_ELT(t, i))); // translate: for srcref part (PR#16732)
    }
    UNPROTECT(1);
    if (isClosure) {
	if (isByteCode(BODY(s))) Rprintf("<bytecode: %p>\n", BODY(s));
	t = CLOENV(s);
	if (t != R_GlobalEnv)
	    Rprintf("%s\n", EncodeEnvironment(t));
    }
}

static
void PrintClosure(SEXP s, Rboolean useSource)
{
    PrintLanguageEtc(s, useSource, TRUE);
}

static
void PrintLanguage(SEXP s, Rboolean useSource)
{
    PrintLanguageEtc(s, useSource, FALSE);
}

/* .Internal(print.default(x, digits, quote, na.print, print.gap,
			   right, max, useS4)) */
SEXP attribute_hidden do_printdefault(/*const*/ Expression* call, const BuiltInFunction* op, Environment* rho, RObject* const* args, int num_args, const PairList* tags)
{
    SEXP x, naprint;
    int tryS4;
    Rboolean callShow = FALSE;

    Rf_PrintDefaults();

    x = args[0]; args = (args + 1);

    if(!Rf_isNull(args[0])) {
	R_print.digits = Rf_asInteger(args[0]);
	if (R_print.digits == NA_INTEGER ||
	    R_print.digits < R_MIN_DIGITS_OPT ||
	    R_print.digits > R_MAX_DIGITS_OPT)
	    Rf_error(_("invalid '%s' argument"), "digits");
    }
    args = (args + 1);

    R_print.quote = Rf_asLogical(args[0]);
    if(R_print.quote == NA_LOGICAL)
	Rf_error(_("invalid '%s' argument"), "quote");
    args = (args + 1);

    naprint = args[0];
    if(!Rf_isNull(naprint))  {
	if(!Rf_isString(naprint) || LENGTH(naprint) < 1)
	    Rf_error(_("invalid 'na.print' specification"));
	R_print.na_string = R_print.na_string_noquote = STRING_ELT(naprint, 0);
	R_print.na_width = R_print.na_width_noquote =
	    int(strlen(R_CHAR(R_print.na_string)));
    }
    args = (args + 1);

    if(!Rf_isNull(args[0])) {
	R_print.gap = Rf_asInteger(args[0]);
	if (R_print.gap == NA_INTEGER || R_print.gap < 0)
	    Rf_error(_("'gap' must be non-negative integer"));
    }
    args = (args + 1);

    R_print.right = Rprt_adj(Rf_asLogical(args[0])); /* Should this be Rf_asInteger()? */
    if(R_print.right == NA_LOGICAL)
	Rf_error(_("invalid '%s' argument"), "right");
    args = (args + 1);

    if(!Rf_isNull(args[0])) {
	R_print.max = Rf_asInteger(args[0]);
	if(R_print.max == NA_INTEGER || R_print.max < 0)
	    Rf_error(_("invalid '%s' argument"), "max");
	else if(R_print.max == INT_MAX) R_print.max--; // so we can add
    }
    args = (args + 1);

    R_print.useSource = Rf_asLogical(args[0]);
    if(R_print.useSource == NA_LOGICAL)
	Rf_error(_("invalid '%s' argument"), "useSource");
    if(R_print.useSource) R_print.useSource = USESOURCE;
    args = (args + 1);

    tryS4 = Rf_asLogical(args[0]);
    if(tryS4 == NA_LOGICAL)
	Rf_error(_("invalid 'tryS4' internal argument"));

    if(tryS4 && IS_S4_OBJECT(x) && isMethodsDispatchOn())
	callShow = TRUE;

    if(callShow) {
	/* we need to get show from the methods namespace if it is
	   not visible on the search path. */
	SEXP call, showS;
	showS = Rf_findVar(Rf_install("show"), rho);
	if(showS == R_UnboundValue) {
	    SEXP methodsNS = R_FindNamespace(Rf_mkString("methods"));
	    if(methodsNS == R_UnboundValue)
		Rf_error("missing methods namespace: this should not happen");
	    PROTECT(methodsNS);
	    showS = Rf_findVarInFrame3(methodsNS, Rf_install("show"), TRUE);
	    UNPROTECT(1);
	    if(showS == R_UnboundValue)
		Rf_error("missing show() in methods namespace: this should not happen");
	}
	PROTECT(call = Rf_lang2(showS, x));
	Rf_eval(call, rho);
	UNPROTECT(1);
    } else {
	Rf_CustomPrintValue(x, rho);
    }

    Rf_PrintDefaults(); /* reset, as na.print etc may have been set */
    return x;
}/* do_printdefault */


/* FIXME : We need a general mechanism for "rendering" symbols. */
/* It should make sure that it quotes when there are special */
/* characters and also take care of ansi escapes properly. */

static void PrintGenericVector(SEXP s, SEXP env)
{
    int i, taglen, ns, w, d, e, wr, dr, er, wi, di, ei;
    SEXP dims, t, names, newcall, tmp;
    char pbuf[115], *ptag, save[TAGBUFLEN0];

    ns = Rf_length(s);
    if((dims = Rf_getAttrib(s, R_DimSymbol)) != R_NilValue && Rf_length(dims) > 1) {
	// special case: array-like list
	PROTECT(dims);
	PROTECT(t = Rf_allocArray(STRSXP, dims));
	/* FIXME: check (ns <= R_print.max +1) ? ns : R_print.max; */
	for (i = 0; i < ns; i++) {
	    switch(TYPEOF(PROTECT(tmp = VECTOR_ELT(s, i)))) {
	    case NILSXP:
		snprintf(pbuf, 115, "NULL");
		break;
	    case LGLSXP:
		if (LENGTH(tmp) == 1) {
		    const int *x = LOGICAL_RO(tmp);
		    formatLogical(x, 1, &w);
		    snprintf(pbuf, 115, "%s",
			     EncodeLogical(x[0], w));
		} else
		    snprintf(pbuf, 115, "Logical,%d", LENGTH(tmp));
		break;
	    case INTSXP:
		/* factors are stored as integers */
		if (Rf_inherits(tmp, "factor")) {
		    snprintf(pbuf, 115, "factor,%d", LENGTH(tmp));
		} else {
		    if (LENGTH(tmp) == 1) {
			const int *x = INTEGER_RO(tmp);
			formatInteger(x, 1, &w);
			snprintf(pbuf, 115, "%s",
				 EncodeInteger(x[0], w));
		    } else
			snprintf(pbuf, 115, "Integer,%d", LENGTH(tmp));
		}
		break;
	    case REALSXP:
		if (LENGTH(tmp) == 1) {
		    const double *x = REAL_RO(tmp);
		    formatReal(x, 1, &w, &d, &e, 0);
		    snprintf(pbuf, 115, "%s",
			     EncodeReal0(x[0], w, d, e, OutDec));
		} else
		    snprintf(pbuf, 115, "Numeric,%d", LENGTH(tmp));
		break;
	    case CPLXSXP:
		if (LENGTH(tmp) == 1) {
		    const Rcomplex *x = COMPLEX_RO(tmp);
		    if (ISNA(x[0].r) || ISNA(x[0].i))
			/* formatReal(NA) --> w=R_print.na_width, d=0, e=0 */
			snprintf(pbuf, 115, "%s",
				 EncodeReal0(NA_REAL, R_print.na_width, 0, 0, OutDec));
		    else {
			formatComplex(x, 1, &wr, &dr, &er, &wi, &di, &ei, 0);
			snprintf(pbuf, 115, "%s",
				 EncodeComplex(x[0],
					       wr, dr, er, wi, di, ei, OutDec));
		    }
		} else
		snprintf(pbuf, 115, "Complex,%d", LENGTH(tmp));
		break;
	    case STRSXP:
		if (LENGTH(tmp) == 1) {
		    const void *vmax = vmaxget();
		    /* This can potentially overflow */
		    const char *ctmp = Rf_translateChar(STRING_ELT(tmp, 0));
		    int len = int(strlen(ctmp));
		    if(len < 100)
			snprintf(pbuf, 115, "\"%s\"", ctmp);
		    else {
			snprintf(pbuf, 101, "\"%s\"", ctmp);
			pbuf[100] = '"'; pbuf[101] = '\0';
			strcat(pbuf, " [truncated]");
		    }
		    vmaxset(vmax);
		} else
		snprintf(pbuf, 115, "Character,%d", LENGTH(tmp));
		break;
	    case RAWSXP:
		snprintf(pbuf, 115, "Raw,%d", LENGTH(tmp));
		break;
	    case LISTSXP:
	    case VECSXP:
		snprintf(pbuf, 115, "List,%d", Rf_length(tmp));
		break;
	    case LANGSXP:
		snprintf(pbuf, 115, "Expression");
		break;
	    default:
		snprintf(pbuf, 115, "?");
		break;
	    }
	    UNPROTECT(1); /* tmp */
	    pbuf[114] = '\0';
	    SET_STRING_ELT(t, i, Rf_mkChar(pbuf));
	}
	if (LENGTH(dims) == 2) {
	    SEXP rl, cl;
	    const char *rn, *cn;
	    Rf_GetMatrixDimnames(s, &rl, &cl, &rn, &cn);
	    /* as from 1.5.0: don't quote here as didn't in array case */
	    printMatrix(t, 0, dims, 0, R_print.right, rl, cl,
			rn, cn);
	}
	else {
	    names = Rf_GetArrayDimnames(s);
	    printArray(t, dims, 0, Rprt_adj_left, names);
	}
	UNPROTECT(2);
    }
    else { // no dim()
	names = Rf_getAttrib(s, R_NamesSymbol);
	taglen = int(strlen(tagbuf));
	ptag = tagbuf + taglen;
        PROTECT(newcall = new Expression(Rf_install("print"), { nullptr }));

	if(ns > 0) {
	    int n_pr = (ns <= R_print.max +1) ? ns : R_print.max;
	    /* '...max +1'  ==> will omit at least 2 ==> plural in msg below */
	    for (i = 0; i < n_pr; i++) {
		if (i > 0) Rprintf("\n");
		if (names != R_NilValue &&
		    STRING_ELT(names, i) != R_NilValue &&
		    *R_CHAR(STRING_ELT(names, i)) != '\0') {
		    const void *vmax = vmaxget();
		    /* Bug for L <- list(`a\\b` = 1, `a\\c` = 2)  :
		       const char *ss = Rf_translateChar(STRING_ELT(names, i));
		    */
		    const char *ss = Rf_EncodeChar(STRING_ELT(names, i));
		    if (taglen + strlen(ss) > TAGBUFLEN) {
			if (taglen <= TAGBUFLEN)
			    sprintf(ptag, "$...");
		    } else {
			/* we need to distinguish character NA from "NA", which
			   is a valid (if non-syntactic) name */
			if (STRING_ELT(names, i) == NA_STRING)
			    sprintf(ptag, "$<NA>");
			else if( Rf_isValidName(ss) )
			    sprintf(ptag, "$%s", ss);
			else
			    sprintf(ptag, "$`%s`", ss);
		    }
		    vmaxset(vmax);
		}
		else {
		    if (taglen + IndexWidth(i) > TAGBUFLEN) {
			if (taglen <= TAGBUFLEN)
			    sprintf(ptag, "$...");
		    } else
			sprintf(ptag, "[[%d]]", i+1);
		}
		Rprintf("%s\n", tagbuf);
		if(Rf_isObject(VECTOR_ELT(s, i))) {
		    /* need to preserve tagbuf */
		    strcpy(save, tagbuf);
		    SETCADR(newcall, VECTOR_ELT(s, i));
		    Rf_eval(newcall, env);
		    strcpy(tagbuf, save);
		}
		else Rf_PrintValueRec(VECTOR_ELT(s, i), env);
		*ptag = '\0';
	    }
	    Rprintf("\n");
	    if(n_pr < ns)
		Rprintf(" [ reached getOption(\"max.print\") -- omitted %d entries ]\n",
			ns - n_pr);
	}
	else { /* ns = length(s) == 0 */
	    const void *vmax = vmaxget();
	    /* Formal classes are represented as empty lists */
	    const char *className = nullptr;
	    SEXP klass;
	    if(Rf_isObject(s) && isMethodsDispatchOn()) {
		klass = Rf_getAttrib(s, R_ClassSymbol);
		if(Rf_length(klass) == 1) {
		    /* internal version of isClass() */
		    char str[201];
		    const char *ss = Rf_translateChar(STRING_ELT(klass, 0));
		    snprintf(str, 200, ".__C__%s", ss);
		    if(Rf_findVar(Rf_install(str), env) != R_UnboundValue)
			className = ss;
		}
	    }
	    if(className) {
		Rprintf("An object of class \"%s\"\n", className);
		UNPROTECT(1);
		printAttributes(s, env, TRUE);
		vmaxset(vmax);
		return;
	    }
	    else {
		if(names != R_NilValue) Rprintf("named ");
		Rprintf("list()\n");
	    }
	    vmaxset(vmax);
	}
	UNPROTECT(1);
    }
    printAttributes(s, env, FALSE);
} // PrintGenericVector


// For pairlist()s only --- the predecessor of PrintGenericVector() above,
// and hence very similar  (and no longer compatible!)
static void printList(SEXP s, SEXP env)
{
    int i, taglen;
    SEXP dims, dimnames, t, newcall;
    char pbuf[101], *ptag;
    const char *rn, *cn;

    if ((dims = Rf_getAttrib(s, R_DimSymbol)) != R_NilValue && Rf_length(dims) > 1) {
	// special case: array-like list
	PROTECT(dims);
	PROTECT(t = Rf_allocArray(STRSXP, dims));
	i = 0;
	while(s != R_NilValue) {
	    switch(TYPEOF(CAR(s))) {

	    case NILSXP:
		snprintf(pbuf, 100, "NULL");
		break;

	    case LGLSXP:
		snprintf(pbuf, 100, "Logical,%d", LENGTH(CAR(s)));
		break;

	    case INTSXP:
	    case REALSXP:
		snprintf(pbuf, 100, "Numeric,%d", LENGTH(CAR(s)));
		break;

	    case CPLXSXP:
		snprintf(pbuf, 100, "Complex,%d", LENGTH(CAR(s)));
		break;

	    case STRSXP:
		snprintf(pbuf, 100, "Character,%d", LENGTH(CAR(s)));
		break;

	    case RAWSXP:
		snprintf(pbuf, 100, "Raw,%d", LENGTH(CAR(s)));
		break;

	    case LISTSXP:
		snprintf(pbuf, 100, "List,%d", Rf_length(CAR(s)));
		break;

	    case LANGSXP:
		snprintf(pbuf, 100, "Expression");
		break;

	    default:
		snprintf(pbuf, 100, "?");
		break;
	    }
	    pbuf[100] ='\0';
	    SET_STRING_ELT(t, i++, Rf_mkChar(pbuf));
	    s = CDR(s);
	}
	if (LENGTH(dims) == 2) {
	    SEXP rl, cl;
	    Rf_GetMatrixDimnames(s, &rl, &cl, &rn, &cn);
	    printMatrix(t, 0, dims, R_print.quote, R_print.right, rl, cl,
			rn, cn);
	}
	else {
	    dimnames = Rf_getAttrib(s, R_DimNamesSymbol);
	    printArray(t, dims, 0, Rprt_adj_left, dimnames);
	}
	UNPROTECT(2);
    }
    else { // no dim()
	i = 1;
	taglen = int(strlen(tagbuf));
	ptag = tagbuf + taglen;
        PROTECT(newcall = new Expression(Rf_install("print"), { nullptr }));
	while (TYPEOF(s) == LISTSXP) {
	    if (i > 1) Rprintf("\n");
	    if (TAG(s) != R_NilValue && Rf_isSymbol(TAG(s))) {
		if (taglen + strlen(R_CHAR(PRINTNAME(TAG(s)))) > TAGBUFLEN) {
		    if (taglen <= TAGBUFLEN)
			sprintf(ptag, "$...");
		} else {
		    /* we need to distinguish character NA from "NA", which
		       is a valid (if non-syntactic) name */
		    if (PRINTNAME(TAG(s)) == NA_STRING)
			sprintf(ptag, "$<NA>");
		    else if( Rf_isValidName(R_CHAR(PRINTNAME(TAG(s)))) )
			sprintf(ptag, "$%s", R_CHAR(PRINTNAME(TAG(s))));
		    else
			sprintf(ptag, "$`%s`", Rf_EncodeChar(PRINTNAME(TAG(s))));
		}
	    }
	    else {
		if (taglen + IndexWidth(i) > TAGBUFLEN) {
		    if (taglen <= TAGBUFLEN)
			sprintf(ptag, "$...");
		} else
		    sprintf(ptag, "[[%d]]", i);
	    }
	    Rprintf("%s\n", tagbuf);
	    if(Rf_isObject(CAR(s))) {
		SETCADR(newcall, CAR(s));
		Rf_eval(newcall, env);
	    }
	    else Rf_PrintValueRec(CAR(s),env);
	    *ptag = '\0';
	    s = CDR(s);
	    i++;
	}
	if (s != R_NilValue) {
	    Rprintf("\n. \n\n");
	    Rf_PrintValueRec(s,env);
	}
	Rprintf("\n");
	UNPROTECT(1);
    }
    printAttributes(s, env, FALSE);
}

static void PrintExpression(SEXP s)
{
    SEXP u;
    int i, n;

    u = Rf_deparse1w(s, FALSE, R_print.useSource | DEFAULTDEPARSE);
    n = LENGTH(u);
    for (i = 0; i < n; i++)
	Rprintf("%s\n", R_CHAR(STRING_ELT(u, i))); /*translated */
}

static void PrintSpecial(SEXP s)
{
    /* This is OK as .Internals are not visible to be printed */
    const char *nm = PRIMNAME(s);
    SEXP env, s2;
    PROTECT_INDEX xp;
    PROTECT_WITH_INDEX(env = Rf_findVarInFrame3(R_BaseEnv,
					     Rf_install(".ArgsEnv"), TRUE),
		       &xp);
    if (TYPEOF(env) == PROMSXP) REPROTECT(env = Rf_eval(env, R_BaseEnv), xp);
    s2 = Rf_findVarInFrame3(env, Rf_install(nm), TRUE);
    if(s2 == R_UnboundValue) {
	REPROTECT(env = Rf_findVarInFrame3(R_BaseEnv,
					Rf_install(".GenericArgsEnv"), TRUE),
		  xp);
	if (TYPEOF(env) == PROMSXP)
	    REPROTECT(env = Rf_eval(env, R_BaseEnv), xp);
	s2 = Rf_findVarInFrame3(env, Rf_install(nm), TRUE);
    }
    if(s2 != R_UnboundValue) {
	SEXP t;
	PROTECT(s2);
	t = Rf_deparse1(s2, FALSE, DEFAULTDEPARSE);
	Rprintf("%s ", R_CHAR(STRING_ELT(t, 0))); /* translated */
	Rprintf(".Primitive(\"%s\")\n", PRIMNAME(s));
	UNPROTECT(1);
    } else /* missing definition, e.g. 'if' */
	Rprintf(".Primitive(\"%s\")\n", PRIMNAME(s));
    UNPROTECT(1);
}

/* PrintValueRec -- recursively print an SEXP

 * This is the "dispatching" function for  print.default()
 */
void attribute_hidden Rf_PrintValueRec(SEXP s, SEXP env)
{
    SEXP t;

#ifdef Win32
    WinCheckUTF8();
#endif
    if(!isMethodsDispatchOn() && (IS_S4_OBJECT(s) || TYPEOF(s) == S4SXP) ) {
	SEXP cl = Rf_getAttrib(s, R_ClassSymbol);
	if(Rf_isNull(cl)) {
	    /* This might be a mistaken S4 bit set */
	    if(TYPEOF(s) == S4SXP)
		Rprintf("<S4 object without a class>\n");
	    else
		Rprintf("<Object of type '%s' with S4 bit but without a class>\n",
			Rf_type2char(TYPEOF(s)));
	} else {
	    SEXP pkg = Rf_getAttrib(s, R_PackageSymbol);
	    if(Rf_isNull(pkg)) {
		Rprintf("<S4 object of class \"%s\">\n",
			R_CHAR(STRING_ELT(cl, 0)));
	    } else {
		Rprintf("<S4 object of class \"%s\" from package '%s'>\n",
			R_CHAR(STRING_ELT(cl, 0)), R_CHAR(STRING_ELT(pkg, 0)));
	    }
	}
	return;
    }
    switch (TYPEOF(s)) {
    case NILSXP:
	Rprintf("NULL\n");
	break;
    case SYMSXP: /* Use deparse here to handle backtick quotification
		  * of "weird names" */
	t = Rf_deparse1(s, FALSE, SIMPLEDEPARSE);
	Rprintf("%s\n", R_CHAR(STRING_ELT(t, 0))); /* translated */
	break;
    case SPECIALSXP:
    case BUILTINSXP:
	PrintSpecial(s);
	break;
    case CHARSXP:
	Rprintf("<CHARSXP: ");
	Rprintf("%s", Rf_EncodeString(s, 0, '"', Rprt_adj_left));
	Rprintf(">\n");
	return; /* skip attribute printing for CHARSXP; they are used */
		/* in managing the CHARSXP cache. */
    case EXPRSXP:
	PrintExpression(s);
	break;
    case LANGSXP:
	PrintLanguage(s, FALSE);
	break;
    case CLOSXP:
	PrintClosure(s, FALSE);
	break;
    case ENVSXP:
	Rprintf("%s\n", EncodeEnvironment(s));
	break;
    case PROMSXP:
	Rprintf("<promise: %p>\n", s);
	break;
    case DOTSXP:
	Rprintf("<...>\n");
	break;
    case VECSXP:
	PrintGenericVector(s, env); /* handles attributes/slots */
	return;
    case LISTSXP:
	printList(s,env);
	break;
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case STRSXP:
    case CPLXSXP:
    case RAWSXP:
	PROTECT(t = Rf_getAttrib(s, R_DimSymbol));
	if (TYPEOF(t) == INTSXP) {
	    if (LENGTH(t) == 1) {
		const void *vmax = vmaxget();
		PROTECT(t = Rf_getAttrib(s, R_DimNamesSymbol));
		if (t != R_NilValue && VECTOR_ELT(t, 0) != R_NilValue) {
		    SEXP nn = Rf_getAttrib(t, R_NamesSymbol);
		    const char *title = nullptr;

		    if (!Rf_isNull(nn))
			title = Rf_translateChar(STRING_ELT(nn, 0));

		    printNamedVector(s, VECTOR_ELT(t, 0), R_print.quote, title);
		}
		else
		    printVector(s, 1, R_print.quote);
		UNPROTECT(1);
		vmaxset(vmax);
	    }
	    else if (LENGTH(t) == 2) {
		SEXP rl, cl;
		const char *rn, *cn;
		Rf_GetMatrixDimnames(s, &rl, &cl, &rn, &cn);
		printMatrix(s, 0, t, R_print.quote, R_print.right, rl, cl,
			    rn, cn);
	    }
	    else {
		SEXP dimnames;
		dimnames = Rf_GetArrayDimnames(s);
		printArray(s, t, R_print.quote, R_print.right, dimnames);
	    }
	}
	else {
	    UNPROTECT(1);
	    PROTECT(t = Rf_getAttrib(s, R_NamesSymbol));
	    if (t != R_NilValue)
		printNamedVector(s, t, R_print.quote, nullptr);
	    else
		printVector(s, 1, R_print.quote);
	}
	UNPROTECT(1);
	break;
    case EXTPTRSXP:
	Rprintf("<pointer: %p>\n", R_ExternalPtrAddr(s));
	break;
    case BCODESXP:
	Rprintf("<bytecode: %p>\n", s);
	break;
    case WEAKREFSXP:
	Rprintf("<weak reference>\n");
	break;
    case S4SXP:
	/*  we got here because no show method, usually no class.
	    Print the "slots" as attributes, since we don't know the class.
	*/
	Rprintf("<S4 Type Object>\n");
	break;
    default:
	UNIMPLEMENTED_TYPE("PrintValueRec", s);
    }
    printAttributes(s, env, FALSE);
#ifdef Win32
    WinUTF8out = FALSE;
#endif
}

/* 2000-12-30 PR#715: remove list tags from tagbuf here
   to avoid $a$battr("foo").  Need to save and restore, since
   attributes might be lists with attributes or just have attributes ...
 */
static void printAttributes(SEXP s, SEXP env, Rboolean useSlots)
{
    SEXP a;
    char *ptag;
    char save[TAGBUFLEN0] = "\0";

    a = ATTRIB(s);
    if (a != R_NilValue) {
	/* guard against cycles through attributes on environments */
	if (strlen(tagbuf) > TAGBUFLEN0)
	    Rf_error(_("print buffer overflow"));
	strcpy(save, tagbuf);
	/* remove the tag if it looks like a list not an attribute */
	if (strlen(tagbuf) > 0 &&
	    *(tagbuf + strlen(tagbuf) - 1) != ')')
	    tagbuf[0] = '\0';
	ptag = tagbuf + strlen(tagbuf);
	while (a != R_NilValue) {
	    if(useSlots && TAG(a) == R_ClassSymbol)
		    goto nextattr;
	    if(Rf_isArray(s) || Rf_isList(s)) {
		if(TAG(a) == R_DimSymbol ||
		   TAG(a) == R_DimNamesSymbol)
		    goto nextattr;
	    }
	    if(Rf_inherits(s, "factor")) {
		if(TAG(a) == R_LevelsSymbol)
		    goto nextattr;
		if(TAG(a) == R_ClassSymbol)
		    goto nextattr;
	    }
	    if(Rf_isFrame(s)) {
		if(TAG(a) == R_RowNamesSymbol)
		    goto nextattr;
	    }
	    if(!Rf_isArray(s)) {
		if (TAG(a) == R_NamesSymbol)
		    goto nextattr;
	    }
	    if(TAG(a) == R_CommentSymbol || TAG(a) == R_SrcrefSymbol
	       || TAG(a) == R_WholeSrcrefSymbol || TAG(a) == R_SrcfileSymbol)
		goto nextattr;
	    if(useSlots)
		sprintf(ptag, "Slot \"%s\":", Rf_EncodeChar(PRINTNAME(TAG(a))));
	    else
		sprintf(ptag, "attr(,\"%s\")", Rf_EncodeChar(PRINTNAME(TAG(a))));
	    Rprintf("%s", tagbuf); Rprintf("\n");
	    if (TAG(a) == R_RowNamesSymbol) {
		/* need special handling AND protection */
		SEXP val;
		PROTECT(val = Rf_getAttrib(s, R_RowNamesSymbol));
		Rf_PrintValueRec(val, env);
		UNPROTECT(1);
		goto nextattr;
	    }
	    if (isMethodsDispatchOn() && IS_S4_OBJECT(CAR(a))) {
		SEXP s, showS;

		showS = Rf_findVar(Rf_install("show"), env);
		if(showS == R_UnboundValue) {
		    SEXP methodsNS = R_FindNamespace(Rf_mkString("methods"));
		    if(methodsNS == R_UnboundValue)
			Rf_error("missing methods namespace: this should not happen");
		    PROTECT(showS);
		    PROTECT(methodsNS);
		    showS = Rf_findVarInFrame3(methodsNS, Rf_install("show"), TRUE);
		    UNPROTECT(2);
		    if(showS == R_UnboundValue)
			Rf_error("missing show() in methods namespace: this should not happen");
		}
		PROTECT(s = Rf_lang2(showS, CAR(a)));
		Rf_eval(s, env);
		UNPROTECT(1);
	    } else if (Rf_isObject(CAR(a))) {
		/* Need to construct a call to
		   print(CAR(a), digits)
		   based on the R_print structure, then Rf_eval(call, env).
		   See do_docall for the template for this sort of thing.

		   quote, right, gap should probably be included if
		   they have non-missing values.

		   This will not dispatch to show() as 'digits' is supplied.
		*/
		SEXP s, na_string = R_print.na_string,
		    na_string_noquote = R_print.na_string_noquote;
		int quote = R_print.quote,
		    digits = R_print.digits, gap = R_print.gap,
		    na_width = R_print.na_width,
		    na_width_noquote = R_print.na_width_noquote;
		Rprt_adj right = Rprt_adj(R_print.right);

                s = PROTECT(new Expression(Rf_install("print"),
                                           { CAR(a), Rf_ScalarInteger(digits) }));
		SET_TAG(CDDR(s), Rf_install("digits"));
		Rf_eval(s, env);
		UNPROTECT(1);
		R_print.quote = quote;
		R_print.right = right;
		R_print.digits = digits;
		R_print.gap = gap;
		R_print.na_width = na_width;
		R_print.na_width_noquote = na_width_noquote;
		R_print.na_string = na_string;
		R_print.na_string_noquote = na_string_noquote;
	    } else
		Rf_PrintValueRec(CAR(a), env);
	nextattr:
	    *ptag = '\0';
	    a = CDR(a);
	}
	strcpy(tagbuf, save);
    }
}/* printAttributes */


/* Print an S-expression using (possibly) local options.
   This is used for auto-printing from main.cpp */

void attribute_hidden Rf_PrintValueEnv(SEXP s, SEXP env)
{
    Rf_PrintDefaults();
    tagbuf[0] = '\0';
    PROTECT(s);
    if(Rf_isObject(s) || Rf_isFunction(s)) {
	/*
	  The intention here is to call show() on S4 objects, otherwise
	  print(), so S4 methods for show() have precedence over those for
	  print() to conform with the "green book", p. 332
	*/
	SEXP call, prinfun;
	SEXP xsym = Rf_install("x");
	if(isMethodsDispatchOn() && IS_S4_OBJECT(s)) {
	    /*
	      Note that can assume there is a loaded "methods"
	      namespace.  It is tempting to cache the value of show in
	      the namespace, but the latter could be unloaded and
	      reloaded in a session.
	    */
	    SEXP methodsNS = R_FindNamespace(Rf_mkString("methods"));
	    if(methodsNS == R_UnboundValue)
		Rf_error("missing methods namespace: this should not happen");
	    PROTECT(methodsNS);
	    prinfun = Rf_findVarInFrame3(methodsNS, Rf_install("show"), TRUE);
	    UNPROTECT(1);
	    if(prinfun == R_UnboundValue)
		Rf_error("missing show() in methods namespace: this should not happen");
	}
	else /* S3 */
	    prinfun = Rf_findVar(Rf_install("print"), R_BaseNamespace);

	/* Bind value to a variable in a local environment, similar to
	   a local({ x <- <value>; print(x) }) call. This avoids
	   problems in previous approaches with value duplication and
	   evaluating the value, which might be a call object. */
	PROTECT(call = Rf_lang2(prinfun, xsym));
	PROTECT(env = Rf_NewEnvironment(R_NilValue, R_NilValue, env));
	Rf_defineVar(xsym, s, env);
	Rf_eval(call, env);
	Rf_defineVar(xsym, R_NilValue, env); /* to eliminate reference to s */
	UNPROTECT(2);
    } else Rf_PrintValueRec(s, env);
    UNPROTECT(1);
}


/* Print an S-expression using global options */

void Rf_PrintValue(SEXP s)
{
    Rf_PrintValueEnv(s, R_GlobalEnv);
}


/* Ditto, but only for objects, for use in debugging */

void R_PV(SEXP s)
{
    if(Rf_isObject(s)) Rf_PrintValueEnv(s, R_GlobalEnv);
}


void attribute_hidden Rf_CustomPrintValue(SEXP s, SEXP env)
{
    tagbuf[0] = '\0';
    Rf_PrintValueRec(s, env);
}


/* xxxpr are mostly for S compatibility (as mentioned in V&R).
   The actual interfaces are now in xxxpr.f
 */

extern "C" {

attribute_hidden
int F77_NAME(dblep0) (const char *label, int *nchar, double *data, int *ndata)
{
    int k, nc = *nchar;

    if(nc < 0) nc = int(strlen(label));
    if(nc > 255) {
	Rf_warning(_("invalid character length in 'dblepr'"));
	nc = 0;
    } else if(nc > 0) {
	for (k = 0; k < nc; k++)
	    Rprintf("%c", label[k]);
	Rprintf("\n");
    }
    if(*ndata > 0) printRealVector(data, *ndata, 1);
    return(0);
}

attribute_hidden
int F77_NAME(intpr0) (const char *label, int *nchar, int *data, int *ndata)
{
    int k, nc = *nchar;

    if(nc < 0) nc = int(strlen(label));
    if(nc > 255) {
	Rf_warning(_("invalid character length in 'intpr'"));
	nc = 0;
    } else if(nc > 0) {
	for (k = 0; k < nc; k++)
	    Rprintf("%c", label[k]);
	Rprintf("\n");
    }
    if(*ndata > 0) printIntegerVector(data, *ndata, 1);
    return(0);
}

attribute_hidden
int F77_NAME(realp0) (const char *label, int *nchar, float *data, int *ndata)
{
    int k, nc = *nchar, nd = *ndata;
    double *ddata;

    if(nc < 0) nc = int(strlen(label));
    if(nc > 255) {
	Rf_warning(_("invalid character length in 'realpr'"));
	nc = 0;
    }
    else if(nc > 0) {
	for (k = 0; k < nc; k++)
	    Rprintf("%c", label[k]);
	Rprintf("\n");
    }
    if(nd > 0) {
	ddata = (double *) malloc(nd*sizeof(double));
	if(!ddata) Rf_error(_("memory allocation error in 'realpr'"));
	for (k = 0; k < nd; k++) ddata[k] = (double) data[k];
	printRealVector(ddata, nd, 1);
	free(ddata);
    }
    return(0);
}

/* Fortran-callable error routine for lapack */

void NORET F77_NAME(xerbla)(const char *srname, int *info)
{
   /* srname is not null-terminated.  It should be 6 characters. */
    char buf[7];
    strncpy(buf, srname, 6);
    buf[6] = '\0';
    Rf_error(_("BLAS/LAPACK routine '%6s' gave error code %d"), buf, -(*info));
}

} /* extern "C" */
