/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-1998  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2016  The R Core Team.
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


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Localization.h>
#include <Internal.h>
#include <Print.h>
#include <Fileio.h>
#include <Rconnections.h>
#include "rho/ArgMatcher.hpp"
#include "rho/ClosureContext.hpp"
#include "rho/ExpressionVector.hpp"

#include <R_ext/RS.h> /* for Memzero */

using namespace rho;

attribute_hidden
R_xlen_t asVecSize(SEXP x)
{
    if (Rf_isVectorAtomic(x) && LENGTH(x) >= 1) {
	switch (TYPEOF(x)) {
	case INTSXP:
	{
	    int res = INTEGER(x)[0];
	    if(res == NA_INTEGER) Rf_error(_("vector size cannot be NA"));
	    return R_xlen_t( res);
	}
	case REALSXP:
	{
	    double d = REAL(x)[0];
	    if(ISNAN(d)) Rf_error(_("vector size cannot be NA/NaN"));
	    if(!R_FINITE(d)) Rf_error(_("vector size cannot be infinite"));
	    if(d > R_XLEN_T_MAX) Rf_error(_("vector size specified is too large"));
	    return R_xlen_t( d);
	}
	case STRSXP:
	{
	    double d = Rf_asReal(x);
	    if(ISNAN(d)) Rf_error(_("vector size cannot be NA/NaN"));
	    if(!R_FINITE(d)) Rf_error(_("vector size cannot be infinite"));
	    if(d > R_XLEN_T_MAX) Rf_error(_("vector size specified is too large"));
	    return R_xlen_t( d);
	}
	default:
	    ;  // -Wswitch
	}
    }
    return -999;  /* which gives error in the caller */
}

SEXP attribute_hidden do_delayed(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* value_, RObject* eval_env_, RObject* assign_env_)
{
    if (!Rf_isString(x_) || Rf_length(x_) == 0)
	Rf_error(_("invalid first argument"));
    SEXP name = Rf_installTrChar(STRING_ELT(x_, 0));
    SEXP expr = value_;

    Environment* eenv = downcast_to_env(eval_env_);
    if (!eenv)
	Rf_error(_("invalid '%s' argument"), "eval.env");

    Environment* aenv = downcast_to_env(assign_env_);
    if (!aenv)
	Rf_error(_("invalid '%s' argument"), "assign.env");

    Rf_defineVar(name, Rf_mkPROMISE(expr, eenv), aenv);
    return R_NilValue;
}

/* makeLazy(names, values, expr, eenv, aenv) */
SEXP attribute_hidden do_makelazy(/*const*/ Expression* call, const BuiltInFunction* op, RObject* vars_, RObject* vals_, RObject* expr_, RObject* db_, RObject* envir_)
{
    SEXP names, values, val, expr, eenv, aenv, expr0;
    R_xlen_t i;

    names = vars_;
    if (!Rf_isString(names))
	Rf_error(_("invalid first argument"));
    values = vals_;
    expr = expr_;
    eenv = db_;
    if (!Rf_isEnvironment(eenv)) Rf_error(_("invalid '%s' argument"), "eval.env");
    aenv = envir_;
    if (!Rf_isEnvironment(aenv)) Rf_error(_("invalid '%s' argument"), "assign.env");

    for(i = 0; i < XLENGTH(names); i++) {
	SEXP name = Rf_installTrChar(STRING_ELT(names, i));
	PROTECT(val = Rf_eval(VECTOR_ELT(values, i), eenv));
	PROTECT(expr0 = Rf_duplicate(expr));
	SETCAR(CDR(expr0), val);
	Rf_defineVar(name, Rf_mkPROMISE(expr0, eenv), aenv);
	UNPROTECT(2);
    }
    return R_NilValue;
}

/* This is a primitive SPECIALSXP */
SEXP attribute_hidden do_onexit(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    ClosureContext *ctxt;
    SEXP code, oldcode, tmp, add;
    int addit = 0;

    checkArity(op, args);
    static GCRoot<ArgMatcher> matcher = new ArgMatcher({ "expr", "add" });
    ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::RAW);
    matcher->match(arglist, { &code, &add });
    if (code == R_MissingArg)
	code = R_NilValue;
    if (add != R_MissingArg) {
	addit = Rf_asLogical(Rf_eval(add, rho));
	if (addit == NA_INTEGER)
	    Rf_errorcall(call, _("invalid '%s' argument"), "add");
    }

    ctxt = ClosureContext::innermost();
    /* Search for the context to which the on.exit action is to be
       attached. Lexical scoping is implemented by searching for the
       first closure call context with an environment matching the
       expression evaluation environment. */
    while (ctxt && ctxt->workingEnvironment() != rho)
	ctxt = ClosureContext::innermost(ctxt->nextOut());
    if (ctxt)
    {
	if (addit && (oldcode = ctxt->onExit()) != R_NilValue ) {
	    if ( CAR(oldcode) != R_BraceSymbol )
	    {
                 PROTECT(tmp = new Expression(R_BraceSymbol,
                                              { oldcode, code }));
		ctxt->setOnExit(tmp);
		UNPROTECT(1);
	    }
	    else
	    {
		PROTECT(tmp = Rf_allocList(1));
		SETCAR(tmp, code);
		ctxt->setOnExit(Rf_listAppend(Rf_duplicate(oldcode),tmp));
		UNPROTECT(1);
	    }
	}
	else
	    ctxt->setOnExit(code);
    }
    UNPROTECT(1);
    return R_NilValue;
}

SEXP attribute_hidden do_args(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP s;

    if (TYPEOF(CAR(args)) == STRSXP && Rf_length(CAR(args))==1) {
	PROTECT(s = Rf_installTrChar(STRING_ELT(CAR(args), 0)));
	SETCAR(args, Rf_findFun(s, rho));
	UNPROTECT(1);
    }

    if (TYPEOF(CAR(args)) == CLOSXP) {
	return Rf_mkCLOSXP(FORMALS(CAR(args)), nullptr, R_GlobalEnv);
    }

    if (TYPEOF(CAR(args)) == BUILTINSXP || TYPEOF(CAR(args)) == SPECIALSXP) {
	RHOCONST char *nm = PRIMNAME(CAR(args));
	SEXP env, s2;
	PROTECT_INDEX xp;

	PROTECT_WITH_INDEX(env = Rf_findVarInFrame3(R_BaseEnv,
						 Rf_install(".ArgsEnv"), TRUE),
			   &xp);

	if (TYPEOF(env) == PROMSXP) REPROTECT(env = Rf_eval(env, R_BaseEnv), xp);
	PROTECT(s2 = Rf_findVarInFrame3(env, Rf_install(nm), TRUE));
	if(s2 != R_UnboundValue) {
	    s = Rf_duplicate(s2);
	    SET_CLOENV(s, R_GlobalEnv);
	    UNPROTECT(2);
	    return s;
	}
	UNPROTECT(1); /* s2 */
	REPROTECT(env = Rf_findVarInFrame3(R_BaseEnv, Rf_install(".GenericArgsEnv"),
					TRUE), xp);
	if (TYPEOF(env) == PROMSXP) REPROTECT(env = Rf_eval(env, R_BaseEnv), xp);
	PROTECT(s2 = Rf_findVarInFrame3(env, Rf_install(nm), TRUE));
	if(s2 != R_UnboundValue) {
	    s = Rf_mkCLOSXP(FORMALS(s2), nullptr, R_GlobalEnv);
	    UNPROTECT(2);
	    return s;
	}
	UNPROTECT(2);
    }
    return R_NilValue;
}

SEXP attribute_hidden do_formals(/*const*/ Expression* call, const BuiltInFunction* op, RObject* fun_)
{
    if (TYPEOF(fun_) == CLOSXP) {
	SEXP f = FORMALS(fun_);
	if (NAMED(fun_) > NAMED(f))
	    SET_NAMED(f, NAMED(fun_));
	return f;
    } else {
	if(!(TYPEOF(fun_) == BUILTINSXP ||
	     TYPEOF(fun_) == SPECIALSXP))
	    Rf_warningcall(call, _("argument is not a function"));
	return R_NilValue;
    }
}

SEXP attribute_hidden do_body(/*const*/ Expression* call, const BuiltInFunction* op, RObject* fun_)
{
    if (TYPEOF(fun_) == CLOSXP) {
	SEXP b = BODY_EXPR(fun_);
	if (NAMED(fun_) > NAMED(b))
	    SET_NAMED(b, NAMED(fun_));
	return b;
    } else {
	if(!(TYPEOF(fun_) == BUILTINSXP ||
	     TYPEOF(fun_) == SPECIALSXP))
	    Rf_warningcall(call, _("argument is not a function"));
	return R_NilValue;
    }
}

namespace rho {
    // In envir.cpp
    Environment* simple_as_environment(RObject* arg, bool allow_null = false);
}

SEXP attribute_hidden do_envir(/*const*/ Expression* call, const BuiltInFunction* op, RObject* fun_)
{
    if (TYPEOF(fun_) == CLOSXP)
	return CLOENV(fun_);
    else if (fun_ == R_NilValue)
	return ClosureContext::innermost()->callEnvironment();
    else return Rf_getAttrib(fun_, R_DotEnvSymbol);
}

SEXP attribute_hidden do_envirgets(/*const*/ Expression* call, const BuiltInFunction* op, RObject* s, RObject* env)
{
    if (TYPEOF(s) == CLOSXP && (env = simple_as_environment(env)) != nullptr) {
	if(MAYBE_SHARED(s))
	    /* this copies but does not duplicate args or code */
	    s = Rf_duplicate(s);
	SET_CLOENV(s, env);
    } else {
	if (env && (env = simple_as_environment(env)) == nullptr)
	    Rf_error(_("replacement object is not an environment"));
	Rf_setAttrib(s, R_DotEnvSymbol, env);
    }
    return s;
}


/** do_newenv() :  .Internal(new.env(hash, parent, size))
 *
 * @return a newly created environment()
 */
SEXP attribute_hidden do_newenv(/*const*/ Expression* call, const BuiltInFunction* op, RObject* hash_, RObject* parent_, RObject* size_)
{
    SEXP size, ans;
    int hash;

    hash = Rf_asInteger(hash_);
    Environment* enclos = simple_as_environment(parent_);
    if (!enclos)
	Rf_error(_("'enclos' must be an environment"));

    if( hash ) {
	PROTECT(size = Rf_coerceVector(size_, INTSXP));
	if (INTEGER(size)[0] == NA_INTEGER)
	    INTEGER(size)[0] = 0; /* so it will use the internal default */
	ans = R_NewHashedEnv(enclos, size);
	UNPROTECT(1);
    } else
	ans = Rf_NewEnvironment(R_NilValue, R_NilValue, enclos);
    return ans;
}

SEXP attribute_hidden do_parentenv(/*const*/ Expression* call, const BuiltInFunction* op, RObject* env_)
{
    Environment* arg = simple_as_environment(env_);
    if (!arg)
	Rf_error( _("argument is not an environment"));
    if( arg == R_EmptyEnv )
	Rf_error(_("the empty environment has no parent"));
    return( ENCLOS(arg) );
}

static Rboolean R_IsImportsEnv(SEXP env)
{
    if (Rf_isNull(env) || !Rf_isEnvironment(env))
	return FALSE;
    if (ENCLOS(env) != R_BaseNamespace)
	return FALSE;
    SEXP name = Rf_getAttrib(env, R_NameSymbol);
    if (!Rf_isString(name) || Rf_length(name) != 1)
	return FALSE;

    const char *imports_prefix = "imports:";
    const char *name_string = R_CHAR(STRING_ELT(name, 0));
    if (streqln(name_string, imports_prefix, strlen(imports_prefix)))
	return TRUE;
    else
	return FALSE;
}

SEXP attribute_hidden do_parentenvgets(/*const*/ Expression* call, const BuiltInFunction* op, RObject* env_, RObject* value_)
{
    Environment* env = simple_as_environment(env_);
    if (!env)
	Rf_error(_("argument is not an environment"));
    if( env == R_EmptyEnv )
	Rf_error(_("can not set parent of the empty environment"));
    if (R_EnvironmentIsLocked(env) && R_IsNamespaceEnv(env))
	Rf_error(_("can not set the parent environment of a namespace"));
    if (R_EnvironmentIsLocked(env) && R_IsImportsEnv(env))
	Rf_error(_("can not set the parent environment of package imports"));

    Environment* parent = simple_as_environment(value_);
    if (!parent)
	Rf_error(_("'parent' is not an environment"));

    env->setEnclosingEnvironment(parent);
    return(env);
}

SEXP attribute_hidden do_envirName(/*const*/ Expression* call, const BuiltInFunction* op, RObject* env_)
{
    SEXP ans=Rf_mkString(""), res;

    if (!env_) {
	return ans;
    }

    Environment* env = simple_as_environment(env_);
    if (env) {
	if (env == R_GlobalEnv) ans = Rf_mkString("R_GlobalEnv");
	else if (env == R_BaseEnv) ans = Rf_mkString("base");
	else if (env == R_EmptyEnv) ans = Rf_mkString("R_EmptyEnv");
	else if (R_IsPackageEnv(env))
	    ans = Rf_ScalarString(STRING_ELT(R_PackageEnvName(env), 0));
	else if (R_IsNamespaceEnv(env))
	    ans = Rf_ScalarString(STRING_ELT(R_NamespaceEnvSpec(env), 0));
	else if (!Rf_isNull(res = Rf_getAttrib(env, R_NameSymbol))) ans = res;
    }
    return ans;
}

#ifdef Win32
# include "rgui_UTF8.h"
#endif
/* Uses R_alloc but called by a .Internal.  Result may be R_alloc-ed */
static const char *trChar(SEXP x)
{
    size_t n = strlen(R_CHAR(x));
    cetype_t ienc = Rf_getCharCE(x);

    if (ienc == CE_BYTES) {
	const char *p = R_CHAR(x), *q;
	char *pp = R_alloc(4*n+1, 1), *qq = pp, buf[5];
	for (q = p; *q; q++) {
	    unsigned char k = static_cast<unsigned char>(*q);
	    if (k >= 0x20 && k < 0x80) {
		*qq++ = *q;
	    } else {
		snprintf(buf, 5, "\\x%02x", k);
		for(int j = 0; j < 4; j++) *qq++ = buf[j];
	    }
	}
	*qq = '\0';
	return pp;
    } else {
#ifdef Win32
	static char buf[106];
	char *p;
	/* Long strings will be rare, and few per cat() call so we
	   can afford to be profligate here: translateChar is */
	if (n < 100) p = buf; else p = R_alloc(n+7, 1);
	if (WinUTF8out && ienc == CE_UTF8) {
	    strcpy(p, UTF8in); strcat(p, R_CHAR(x)); strcat(p, UTF8out);
	    return p;
	} else
#endif
	    return Rf_translateChar(x);
    }
}

static void cat_newline(SEXP labels, int *width, int lablen, int ntot)
{
    Rprintf("\n");
    *width = 0;
    if (labels != R_NilValue) {
	Rprintf("%s ", Rf_EncodeString(STRING_ELT(labels, ntot % lablen),
				    1, 0, Rprt_adj_left));
	*width += Rstrlen(STRING_ELT(labels, ntot % lablen), 0) + 1;
    }
}

static void cat_sepwidth(SEXP sep, int *width, int ntot)
{
    if (sep == R_NilValue || LENGTH(sep) == 0)
	*width = 0;
    else
	*width = Rstrlen(STRING_ELT(sep, ntot % LENGTH(sep)), 0);
}

static void cat_printsep(SEXP sep, int ntot)
{
    const char *sepchar;
    if (sep == R_NilValue || LENGTH(sep) == 0)
	return;

    sepchar = trChar(STRING_ELT(sep, ntot % LENGTH(sep)));
    Rprintf("%s", sepchar);
    return;
}

typedef struct cat_info {
    Rboolean wasopen;
    int changedcon;
    Rconnection con;
} cat_info;

static void cat_cleanup(cat_info* pci)
{
    Rconnection con = pci->con;
    Rboolean wasopen = pci->wasopen;
    int changedcon = pci->changedcon;

    con->fflush(con);
    if(changedcon) switch_stdout(-1, 0);
    /* previous line might have closed it */
    if(!wasopen && con->isopen) con->close(con);
#ifdef Win32
    WinUTF8out = FALSE;
#endif
}

SEXP attribute_hidden do_cat(/*const*/ Expression* call, const BuiltInFunction* op, RObject* dots_, RObject* file_, RObject* sep_, RObject* fill_, RObject* labels_, RObject* append_)
{
    cat_info ci;
    SEXP objs, file, fill, sepr, labs, s;
    int ifile;
    Rconnection con;
    int append;
    int i, iobj, n, nobjs, pwidth, width, sepw, lablen, ntot, nlsep, nlines;
    char buf[512];
    const char *p = "";

    /* Use standard printing defaults */
    Rf_PrintDefaults();

    objs = dots_;

    file = file_;
    ifile = Rf_asInteger(file);
    con = getConnection(ifile);
    if(!con->canwrite) /* if it is not open, we may not know yet */
	Rf_error(_("cannot write to this connection"));

    sepr = sep_;
    if (!Rf_isString(sepr))
	Rf_error(_("invalid '%s' specification"), "sep");
    nlsep = 0;
    for (i = 0; i < LENGTH(sepr); i++)
	if (strstr(R_CHAR(STRING_ELT(sepr, i)), "\n")) nlsep = 1; /* ASCII */

    fill = fill_;
    if ((!Rf_isNumeric(fill) && !Rf_isLogical(fill)) || (Rf_length(fill) != 1))
	Rf_error(_("invalid '%s' argument"), "fill");
    if (Rf_isLogical(fill)) {
	if (Rf_asLogical(fill) == 1)
	    pwidth = R_print.width;
	else
	    pwidth = INT_MAX;
    }
    else pwidth = Rf_asInteger(fill);
    if(pwidth <= 0) {
	Rf_warning(_("non-positive 'fill' argument will be ignored"));
	pwidth = INT_MAX;
    }

    labs = labels_;
    if (!Rf_isString(labs) && labs != R_NilValue)
	Rf_error(_("invalid '%s' argument"), "labels");
    lablen = Rf_length(labs);

    append = Rf_asLogical(append_);
    if (append == NA_LOGICAL)
	Rf_error(_("invalid '%s' specification"), "append");

    ci.wasopen = con->isopen;

    ci.changedcon = switch_stdout(ifile, 0);
    /* will open new connection if required, and check for writeable */
#ifdef Win32
    /* do this after re-sinking output */
    WinCheckUTF8();
#endif

    ci.con = con;

    /* use try-catch to close the connection if there is an error */
    try {
	nobjs = Rf_length(objs);
	width = 0;
	ntot = 0;
	nlines = 0;
	for (iobj = 0; iobj < nobjs; iobj++) {
	    s = VECTOR_ELT(objs, iobj);
	    if (iobj != 0 && !Rf_isNull(s))
		cat_printsep(sepr, ntot++);
	    n = Rf_length(s);
	    /* 0-length objects are ignored */
	    if (n > 0) {
		if (labs != R_NilValue && (iobj == 0)
		    && (Rf_asInteger(fill) > 0)) {
		    Rprintf("%s ", trChar(STRING_ELT(labs, nlines % lablen)));
		    /* FIXME -- Rstrlen allows for double-width chars */
		    width += Rstrlen(STRING_ELT(labs, nlines % lablen), 0) + 1;
		    nlines++;
		}
		if (Rf_isString(s))
		    p = trChar(STRING_ELT(s, 0));
		else if (Rf_isSymbol(s)) /* length 1 */
		    p = R_CHAR(PRINTNAME(s));
		else if (Rf_isVectorAtomic(s)) {
		    /* Not a string, as that is covered above.
		       Thus the maximum size is about 60.
		       The copy is needed as cat_newline might reuse the buffer.
		       Use strncpy is in case these assumptions change.
		    */
		    p = EncodeElement0(s, 0, 0, OutDec);
		    strncpy(buf, p, 512); buf[511] = '\0';
		    p = buf;
		}
#ifdef fixed_cat
		else if (isVectorList(s)) {
		    /* FIXME:	 call EncodeElement() for every element of  s.

		    Real Problem: `s' can be large;
		    should do line breaking etc.. (buf is of limited size)
		    */
		}
#endif
		else
		Rf_error(_("argument %d (type '%s') cannot be handled by 'cat'"),
		      1+iobj, Rf_type2char(TYPEOF(s)));
		/* FIXME : cat(...) should handle ANYTHING */
		size_t w = strlen(p);
		cat_sepwidth(sepr, &sepw, ntot);
		if ((iobj > 0) && (width + w + sepw > RHOCONSTRUCT(size_t, pwidth))) {
		    cat_newline(labs, &width, lablen, nlines);
		    nlines++;
		}
		for (i = 0; i < n; i++, ntot++) {
		    Rprintf("%s", p);
		    width += int(w + sepw);
		    if (i < (n - 1)) {
			cat_printsep(sepr, ntot);
			if (Rf_isString(s))
			    p = trChar(STRING_ELT(s, i+1));
			else {
			    p = EncodeElement0(s, i+1, 0, OutDec);
			    strncpy(buf, p, 512); buf[511] = '\0';
			    p = buf;
			}
			w = int(strlen(p));
			cat_sepwidth(sepr, &sepw, ntot);
			/* This is inconsistent with the version above.
			   As from R 2.3.0, fill <= 0 is ignored. */
			if ((width + w + sepw > RHOCONSTRUCT(size_t, pwidth)) && pwidth) {
			    cat_newline(labs, &width, lablen, nlines);
			    nlines++;
			}
		    } else ntot--; /* we don't print sep after last, so don't advance */
		}
	    }
	}
	if ((pwidth != INT_MAX) || nlsep)
	    Rprintf("\n");
    }
    catch (...) {
	cat_cleanup(&ci);
	throw;
    }

    cat_cleanup(&ci);

    return R_NilValue;
}

SEXP attribute_hidden do_makelist(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int n, havenames;
    /* compute number of args and check for names */
    SEXP next;
    for (next = args, n = 0, havenames = FALSE;
	 next != R_NilValue;
	 next = CDR(next)) {
	if (TAG(next) != R_NilValue)
	    havenames = TRUE;
	n++;
    }

    SEXP list = PROTECT(Rf_allocVector(VECSXP, n));
    SEXP names = PROTECT(havenames ? Rf_allocVector(STRSXP, n) : R_NilValue);
    for (int i = 0; i < n; i++) {
	if (havenames) {
	    if (TAG(args) != R_NilValue)
		SET_STRING_ELT(names, i, PRINTNAME(TAG(args)));
	    else
		SET_STRING_ELT(names, i, R_BlankString);
	}
	if (NAMED(CAR(args)))
	    INCREMENT_NAMED(CAR(args));
	SET_VECTOR_ELT(list, i, CAR(args));
	args = CDR(args);
    }
    if (havenames) {
	Rf_setAttrib(list, R_NamesSymbol, names);
    }
    UNPROTECT(2);
    return list;
}

/* This is a primitive SPECIALSXP */
SEXP attribute_hidden do_expression(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP a, ans, nms;
    int i, n, named;
    named = 0;
    n = Rf_length(args);
    PROTECT(ans = Rf_allocVector(EXPRSXP, n));
    a = args;
    for (i = 0; i < n; i++) {
	if(MAYBE_REFERENCED(CAR(a)))
	    SET_XVECTOR_ELT(ans, i, Rf_duplicate(CAR(a)));
	else
	    SET_XVECTOR_ELT(ans, i, CAR(a));
	if (TAG(a) != R_NilValue) named = 1;
	a = CDR(a);
    }
    if (named) {
	PROTECT(nms = Rf_allocVector(STRSXP, n));
	a = args;
	for (i = 0; i < n; i++) {
	    if (TAG(a) != R_NilValue)
		SET_STRING_ELT(nms, i, PRINTNAME(TAG(a)));
	    else
		SET_STRING_ELT(nms, i, R_BlankString);
	    a = CDR(a);
	}
	Rf_setAttrib(ans, R_NamesSymbol, nms);
	UNPROTECT(1);
    }
    UNPROTECT(1);
    return ans;
}

/* vector(mode="logical", length=0) */
SEXP attribute_hidden do_makevector(/*const*/ Expression* call, const BuiltInFunction* op, RObject* mode_, RObject* length_)
{
    R_xlen_t len;
    SEXP s;
    SEXPTYPE mode;
    if (Rf_length(length_) != 1) Rf_error(_("invalid '%s' argument"), "length");
    len = asVecSize(length_);
    if (len < 0) Rf_error(_("invalid '%s' argument"), "length");
    s = Rf_coerceVector(mode_, STRSXP);
    if (Rf_length(s) != 1) Rf_error(_("invalid '%s' argument"), "mode");
    mode = Rf_str2type(R_CHAR(STRING_ELT(s, 0))); /* ASCII */
    if (RHOCONSTRUCT(int, mode) == -1 && streql(R_CHAR(STRING_ELT(s, 0)), "double"))
	mode = REALSXP;
    switch (mode) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case EXPRSXP:
    case VECSXP:
    case RAWSXP:
	s = Rf_allocVector(mode, len);
	break;
    case LISTSXP:
	if (len > INT_MAX) Rf_error("too long for a pairlist");
	s = Rf_allocList(int(len));
	break;
    default:
	Rf_error(_("vector: cannot make a vector of mode '%s'."),
	      Rf_translateChar(STRING_ELT(s, 0))); /* should be ASCII */
    }
    if (mode == INTSXP || mode == LGLSXP)
	Memzero(INTEGER(s), len);
    else if (mode == REALSXP)
	Memzero(REAL(s), len);
    else if (mode == CPLXSXP)
	Memzero(COMPLEX(s), len);
    else if (mode == RAWSXP)
	Memzero(RAW(s), len);
    /* other cases: list/expression have "NULL", ok */
    return s;
}


/* do_lengthgets: assign a length to a vector or a list */
/* (if it is vectorizable). We could probably be fairly */
/* clever with memory here if we wanted to. */

/* used in connections.cpp */
SEXP Rf_xlengthgets(SEXP x, R_xlen_t len)
{
    R_xlen_t lenx, i;
    SEXP rval, names, xnames, t;
    if (!Rf_isVector(x) && !Rf_isVectorizable(x))
	Rf_error(_("cannot set length of non-vector"));
    lenx = Rf_xlength(x);
    if (lenx == len)
	return (x);
    PROTECT(rval = Rf_allocVector(TYPEOF(x), len));
    PROTECT(xnames = Rf_getAttrib(x, R_NamesSymbol));
    if (xnames != R_NilValue)
	names = Rf_allocVector(STRSXP, len);
    else names = R_NilValue;	/*- just for -Wall --- should we do this ? */
    switch (TYPEOF(x)) {
    case NILSXP:
	break;
    case LGLSXP:
    case INTSXP:
	for (i = 0; i < len; i++)
	    if (i < lenx) {
		INTEGER(rval)[i] = INTEGER(x)[i];
		if (xnames != R_NilValue)
		    SET_STRING_ELT(names, i, STRING_ELT(xnames, i));
	    }
	    else
		INTEGER(rval)[i] = NA_INTEGER;
	break;
    case REALSXP:
	for (i = 0; i < len; i++)
	    if (i < lenx) {
		REAL(rval)[i] = REAL(x)[i];
		if (xnames != R_NilValue)
		    SET_STRING_ELT(names, i, STRING_ELT(xnames, i));
	    }
	    else
		REAL(rval)[i] = NA_REAL;
	break;
    case CPLXSXP:
	for (i = 0; i < len; i++)
	    if (i < lenx) {
		COMPLEX(rval)[i] = COMPLEX(x)[i];
		if (xnames != R_NilValue)
		    SET_STRING_ELT(names, i, STRING_ELT(xnames, i));
	    }
	    else {
		COMPLEX(rval)[i].r = NA_REAL;
		COMPLEX(rval)[i].i = NA_REAL;
	    }
	break;
    case STRSXP:
	for (i = 0; i < len; i++)
	    if (i < lenx) {
		SET_STRING_ELT(rval, i, STRING_ELT(x, i));
		if (xnames != R_NilValue)
		    SET_STRING_ELT(names, i, STRING_ELT(xnames, i));
	    }
	    else
		SET_STRING_ELT(rval, i, NA_STRING);
	break;
    case LISTSXP:
	for (t = rval; t != R_NilValue; t = CDR(t), x = CDR(x)) {
	    SETCAR(t, CAR(x));
	    SET_TAG(t, TAG(x));
	}
    case VECSXP:
	for (i = 0; i < len; i++)
	    if (i < lenx) {
		SET_VECTOR_ELT(rval, i, VECTOR_ELT(x, i));
		if (xnames != R_NilValue)
		    SET_STRING_ELT(names, i, STRING_ELT(xnames, i));
	    }
	break;
    case RAWSXP:
	for (i = 0; i < len; i++)
	    if (i < lenx) {
		RAW(rval)[i] = RAW(x)[i];
		if (xnames != R_NilValue)
		    SET_STRING_ELT(names, i, STRING_ELT(xnames, i));
	    }
	    else
		RAW(rval)[i] = Rbyte( 0);
	break;
    default:
	UNIMPLEMENTED_TYPE("length<-", x);
    }
    if (Rf_isVector(x) && xnames != R_NilValue)
	Rf_setAttrib(rval, R_NamesSymbol, names);
    UNPROTECT(2);
    return rval;
}

/* public older version */
SEXP Rf_lengthgets(SEXP x, R_len_t len)
{
    return Rf_xlengthgets(x, R_xlen_t( len));
}


SEXP attribute_hidden do_lengthgets(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* value_)
{
    SEXP x = x_;

    if (!Rf_isVector(x) && !Rf_isVectorizable(x))
	Rf_error(_("invalid argument"));
    if (Rf_length(value_) != 1)
	Rf_error(_("invalid value"));
    R_xlen_t len = asVecSize(value_);
    if (len < 0) Rf_error(_("invalid value"));
    if (len > R_LEN_T_MAX) {
#ifdef LONG_VECTOR_SUPPORT
	return Rf_xlengthgets(x, len);
#else
	Rf_error(_("vector size specified is too large"));
	return x; /* -Wall */
#endif
    }
    return Rf_lengthgets(x, R_len_t( len));
}

/* Expand dots in args, but do not evaluate */
static SEXP expandDots(SEXP el, SEXP rho)
{
    SEXP ans, tail;

    PROTECT(el); /* in do_switch, this is already protected */
    PROTECT(ans = tail = CONS(R_NilValue, R_NilValue));

    while (el != R_NilValue) {
	if (CAR(el) == R_DotsSymbol) {
	    SEXP h = PROTECT(Rf_findVar(CAR(el), rho));
	    if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
		while (h != R_NilValue) {
		    if (TYPEOF(CAR(h)) == PROMSXP || CAR(h) == R_MissingArg)
		      SETCDR(tail, CONS(CAR(h), R_NilValue));
                    else
		      SETCDR(tail, CONS(Rf_mkPROMISE(CAR(h), rho), R_NilValue));
		    tail = CDR(tail);
		    if(TAG(h) != R_NilValue) SET_TAG(tail, TAG(h));
		    h = CDR(h);
		}
	    } else if (h != R_MissingArg)
		Rf_error(_("'...' used in an incorrect context"));
	    UNPROTECT(1); /* h */
	} else {
	    SETCDR(tail, CONS(CAR(el), R_NilValue));
	    tail = CDR(tail);
	    if(TAG(el) != R_NilValue) SET_TAG(tail, TAG(el));
	}
	el = CDR(el);
    }
    UNPROTECT(2);
    return CDR(ans);
}

/* This function is used in do_switch to record the default value and
   to detect multiple defaults, which are not allowed as of 2.13.x */

static SEXP setDflt(SEXP arg, SEXP dflt)
{
    if (dflt) {
	SEXP dflt1, dflt2;
	PROTECT(dflt1 = Rf_deparse1line(dflt, TRUE));
	PROTECT(dflt2 = Rf_deparse1line(CAR(arg), TRUE));
	Rf_error(_("duplicate 'switch' defaults: '%s' and '%s'"),
	      R_CHAR(STRING_ELT(dflt1, 0)), R_CHAR(STRING_ELT(dflt2, 0)));
	UNPROTECT(2); /* won't get here, but just for good form */
    }
    return(CAR(arg));
}

/* For switch, evaluate the first arg, if it is a character then try
 to match the name with the remaining args, and evaluate the match. If
 the value is missing then take the next non-missing arg as the value.
 Then things like switch(as.character(answer), yes=, YES=1, no=, NO=2,
 3) will work.  But if there is no 'next', return NULL. One arg beyond
 the first is allowed to be unnamed; it becomes the default value if
 there is no match.

 If the value of the first arg is not a character string
 then coerce it to an integer k and choose the kth argument from those
 that remain provided 1 < k < nargs.

 Changed in 2.11.0 to be primitive, so the wrapper does not partially
 match to EXPR, and to return NULL invisibly if it is an error
 condition.

 This is a SPECIALSXP, so arguments need to be evaluated as needed.
  And (see names.cpp) X=2, so it defaults to a visible value.
*/


SEXP attribute_hidden do_switch(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int argval, nargs = Rf_length(args);
    SEXP x, y, z, w, ans, dflt = nullptr;

    PROTECT(x = Rf_eval(CAR(args), rho));
    if (!Rf_isVector(x) || Rf_length(x) != 1)
	Rf_errorcall(call, _("EXPR must be a length 1 vector"));
    if (Rf_isFactor(x))
	Rf_warningcall(call,
		    _("EXPR is a \"factor\", treated as integer.\n"
		      " Consider using '%s' instead."),
		    "switch(as.character( * ), ...)");
    if (nargs > 1) {
	/* There is a complication: if called from lapply
	   there may be a ... argument */
	PROTECT(w = expandDots(CDR(args), rho));
	if (Rf_isString(x)) {
	    for (y = w; y != R_NilValue; y = CDR(y)) {
		if (TAG(y) != R_NilValue) {
		    if (Rf_pmatch(STRING_ELT(x, 0), TAG(y), RHO_TRUE /* exact */)) {
			/* Find the next non-missing argument.
			   (If there is none, return NULL.) */
			while (CAR(y) == R_MissingArg) {
			    y = CDR(y);
			    if (y == R_NilValue) break;
			    if (TAG(y) == R_NilValue) dflt = setDflt(y, dflt);
			}
			if (y == R_NilValue) {
			    R_Visible = FALSE;
			    UNPROTECT(2);
			    return R_NilValue;
			}
			/* Check for multiple defaults following y.  This loop
			   is not necessary to determine the value of the
			   switch(), but it should be fast and will detect
			   typos. */
			for (z = CDR(y); z != R_NilValue; z = CDR(z))
			    if (TAG(z) == R_NilValue) dflt = setDflt(z, dflt);

			ans =  Rf_eval(CAR(y), rho);
			UNPROTECT(2);
			return ans;
		    }
		} else
		    dflt = setDflt(y, dflt);
	    }
	    if (dflt) {
		ans =  Rf_eval(dflt, rho);
		UNPROTECT(2);
		return ans;
	    }
	    /* fall through to error */
	} else { /* Treat as numeric */
	    argval = Rf_asInteger(x);
	    if (argval != NA_INTEGER && argval >= 1 && argval <= Rf_length(w)) {
		SEXP alt = CAR(Rf_nthcdr(w, argval - 1));
		if (alt == R_MissingArg)
		    Rf_error("empty alternative in numeric switch");
		ans =  Rf_eval(alt, rho);
		UNPROTECT(2);
		return ans;
	    }
	    /* fall through to error */
	}
	UNPROTECT(1); /* w */
    } else
	Rf_warningcall(call, _("'switch' with no alternatives"));
    /* an error */
    UNPROTECT(1); /* x */
    R_Visible = FALSE;
    return R_NilValue;
}
