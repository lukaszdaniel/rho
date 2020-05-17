/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2014   The R Core Team.
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

// For debugging:
#include <iostream>

#define R_NO_REMAP

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Localization.h>
#include <Internal.h>

#include "rho/Browser.hpp"
#include "rho/ClosureContext.hpp"
#include "rho/CommandTerminated.hpp"
#include "rho/BuiltInFunction.hpp"

using namespace std;
using namespace rho;

/* R_sysframe - look back up the context stack until the */
/* nth closure context and return that workingEnvironment(). */
/* R_sysframe(0) means the R_GlobalEnv environment */
/* negative n counts back from the current frame */
/* positive n counts up from the globalEnv */

SEXP attribute_hidden R_sysframe(int n, ClosureContext *cptr)
{
    if (n == 0)
	return(R_GlobalEnv);

    if (n == NA_INTEGER) Rf_error(_("NA argument is invalid"));

    if (n > 0)
	n = Rf_framedepth(cptr) - n;
    else
	n = -n;

    if(n < 0)
	Rf_error(_("not that many frames on the stack"));

    while (cptr) {
	if (n == 0) {  /* we need to detach the enclosing env */
	    return cptr->workingEnvironment();
	}
	else
	    n--;
	cptr = ClosureContext::innermost(cptr->nextOut());
    }
    if(n == 0)
	return R_GlobalEnv;
    else
	Rf_error(_("not that many frames on the stack"));
    return nullptr;	   /* just for -Wall */
}


/* We need to find the environment that can be returned by sys.frame */
/* (so it needs to be on the workingEnvironment() pointer of a context) that matches */
/* the environment where the closure arguments are to be evaluated. */
/* It would be much simpler if sysparent just returned cptr->sysparent */
/* but then we wouldn't be compatible with S. */

int attribute_hidden R_sysparent(int n, ClosureContext *cptr)
{
    int j;
    SEXP s;
    if(n <= 0)
	Rf_error(_("only positive values of 'n' are allowed"));
    while (cptr && n > 1) {
	    n--;
	    cptr = ClosureContext::innermost(cptr->nextOut());
    }
    if (!cptr)
	return 0;
    // Foll. 3 lines probably soon redundant in rho:
    s = cptr->callEnvironment();
    if(s == R_GlobalEnv)
	return 0;
    j = 0;
    while (cptr != nullptr ) {
	j++;
	if (cptr->workingEnvironment() == s)
	    n=j;
	cptr = ClosureContext::innermost(cptr->nextOut());
    }
    n = j - n + 1;
    if (n < 0)
	n = 0;
    return n;
}

int attribute_hidden Rf_framedepth(ClosureContext* cptr)
{
    int nframe = 0;
    while (cptr) {
	nframe++;
	cptr = ClosureContext::innermost(cptr->nextOut());
    }
    return nframe;
}

static SEXP getCallWithSrcref(ClosureContext *cptr)
{
    SEXP result;

	    PROTECT(result = Rf_shallow_duplicate(const_cast<Expression*>(cptr->call())));
	    if (cptr->sourceLocation())
		Rf_setAttrib(result, Symbols::SrcrefSymbol,
			  Rf_duplicate(cptr->sourceLocation()));
	    UNPROTECT(1);
	    return result;
}

SEXP attribute_hidden R_syscall(int n, ClosureContext* cptr)
{
    /* negative n counts back from the current frame */
    /* positive n counts up from the globalEnv */

    if (n > 0)
	n = Rf_framedepth(cptr) - n;
    else
	n = -n;
    if (n < 0)
	Rf_error(_("not that many frames on the stack"));
    while (cptr) {
	if (n == 0)
	    return getCallWithSrcref(cptr);
	else
	    n--;
	cptr = ClosureContext::innermost(cptr->nextOut());
    }
    Rf_error(_("not that many frames on the stack"));
    return nullptr; /* just for -Wall */
}

SEXP attribute_hidden R_sysfunction(int n, ClosureContext* cptr)
{
    if (n > 0)
	n = Rf_framedepth(cptr) - n;
    else
	n = - n;
    if (n < 0)
	Rf_error(_("not that many frames on the stack"));
    while (cptr) {
	if (n == 0)
	    return cptr->function()->clone();  /***** do we need to DUP? */
	else
	    n--;
	cptr = ClosureContext::innermost(cptr->nextOut());
    }
    Rf_error(_("not that many frames on the stack"));
    return nullptr;	/* just for -Wall */
}


/* functions to support looking up information about the browser */
/* contexts that are in the evaluation stack */

SEXP attribute_hidden do_sysbrowser(/*const*/ Expression* call, const BuiltInFunction* op, RObject* n_)
{
    int n;

    n = Rf_asInteger(n_);
    if(n < 1) Rf_error(_("number of contexts must be positive"));

    switch (op->variant()) {
    case 1: /* text */
    case 2: /* condition */
	{
	    if (n > int(Browser::numberActive())) {
		if (n == 1)
		    Rf_error(_("no browser context to query"));
		else Rf_error(_("not that many calls to browser are active"));
	    }
	    Browser* browser
		= Browser::fromOutermost(Browser::numberActive() - n);
	    return op->variant() == 1 ? browser->text() : browser->condition();
	}
	break;
    case 3: /* turn on debugging n levels up */
	{
	    if (Browser::numberActive() == 0)
		Rf_error(_("no browser context to query"));
	    Browser* browser
		= Browser::fromOutermost(Browser::numberActive() - 1);
	    ClosureContext* cptr = ClosureContext::innermost(browser->context());
	    while (cptr && n > 1) {
		n--;
		cptr = ClosureContext::innermost(cptr->nextOut());
	    }
	    if (!cptr) {
		Rf_error(_("not that many functions on the call stack"));
        }
		SET_RDEBUG(cptr->workingEnvironment(), TRUE);

	}
        break;
    }
    return nullptr;
}

/* An implementation of S's frame access functions. They usually count */
/* up from the globalEnv while we like to count down from the currentEnv. */
/* So if the argument is negative count down if positive count up. */
/* We don't want to count the closure that do_sys is contained in, so the */
/* indexing is adjusted to handle this. */

SEXP attribute_hidden do_sys(/*const*/ Expression* call, const BuiltInFunction* op, int num_args, ...)
{
    int i, n  = -1, nframe;
    SEXP rval, t;
    ClosureContext *cptr;

    /* first find the context that sys.xxx needs to be evaluated in */
    cptr = R_GlobalContext();
    t = cptr->callEnvironment();
    while (cptr && cptr->workingEnvironment() != t)
	cptr = ClosureContext::innermost(cptr->nextOut());

    if (num_args == 1) {
	UNPACK_VA_ARGS(num_args, (n_));
	n = Rf_asInteger(n_);
    }

    switch (op->variant()) {
    case 1: /* parent */
	if(n == NA_INTEGER)
	    Rf_error(_("invalid '%s' argument"), "n");
	i = nframe = Rf_framedepth(cptr);
	/* This is a pretty awful kludge, but the alternative would be
	   a major redesign of everything... -pd */
	while (n-- > 0)
	    i = R_sysparent(nframe - i + 1, cptr);
	return Rf_ScalarInteger(i);
    case 2: /* call */
	if(n == NA_INTEGER)
	    Rf_error(_("invalid '%s' argument"), "which");
	return R_syscall(n, cptr);
    case 3: /* frame */
	if(n == NA_INTEGER)
	    Rf_error(_("invalid '%s' argument"), "which");
	return R_sysframe(n, cptr);
    case 4: /* sys.nframe */
	return Rf_ScalarInteger(Rf_framedepth(cptr));
    case 5: /* sys.calls */
	nframe = Rf_framedepth(cptr);
	PROTECT(rval = Rf_allocList(nframe));
	t=rval;
	for(i = 1; i <= nframe; i++, t = CDR(t))
	    SETCAR(t, R_syscall(i, cptr));
	UNPROTECT(1);
	return rval;
    case 6: /* sys.frames */
	nframe = Rf_framedepth(cptr);
	PROTECT(rval = Rf_allocList(nframe));
	t = rval;
	for(i = 1; i <= nframe; i++, t = CDR(t))
	    SETCAR(t, R_sysframe(i, cptr));
	UNPROTECT(1);
	return rval;
    case 7: /* sys.on.exit */
    {
	RObject* conexit = cptr ? cptr->onExit() : nullptr;
	if (conexit == nullptr)
	    return nullptr;
	else if (CDR(conexit) == nullptr)
	    return CAR(conexit);
	else
	    return new Expression(Symbols::BraceSymbol, { conexit });
    }
    case 8: /* sys.parents */
	nframe = Rf_framedepth(cptr);
	rval = Rf_allocVector(INTSXP, nframe);
	for(i = 0; i < nframe; i++)
	    INTEGER(rval)[i] = R_sysparent(nframe - i, cptr);
	return rval;
    case 9: /* sys.function */
	if(n == NA_INTEGER)
	    Rf_error(_("invalid '%s' value"), "which");
	return(R_sysfunction(n, cptr));
    default:
	Rf_error(_("internal error in 'do_sys'"));
	return nullptr;/* just for -Wall */
    }
}

SEXP attribute_hidden do_parentframe(/*const*/ Expression* call, const BuiltInFunction* op, RObject* n_)
{
    ClosureContext *cptr;

    int n = Rf_asInteger(n_);

    if(n == NA_INTEGER || n < 1 )
	Rf_error(_("invalid '%s' value"), "n");

    cptr = R_GlobalContext();
    SEXP t = cptr->callEnvironment();
    while (cptr){
	if (cptr->workingEnvironment() == t)
	    {
		if (n == 1)
		    return cptr->callEnvironment();
		n--;
		t = cptr->callEnvironment();
	    }
	cptr = ClosureContext::innermost(cptr->nextOut());
    }
    return R_GlobalEnv;
}

/* R_ToplevelExec - call fun(data) within an Evaluator and
   insure that this function cannot be left by an exception.  R errors in
   the call to fun will result in a jump to top level. The return
   value is TRUE if fun returns normally, FALSE if it results in a
   jump to top level. */

Rboolean R_ToplevelExec(void (*fun)(void *), void *data)
{
    volatile SEXP topExp;//, oldRVal;
    volatile Rboolean oldvis;
    Rboolean result;

    PROTECT(topExp = R_CurrentExpr);
    GCStackRoot<PairList> oldHStack(R_HandlerStack);
    GCStackRoot<PairList> oldRStack(R_RestartStack);
    //GCStackRoot<PairList> oldRVal(R_ReturnedValue);
    oldvis = R_Visible;
    R_HandlerStack = nullptr;
    R_RestartStack = nullptr;

    try {
	fun(data);
	result = TRUE;
    }
    catch (CommandTerminated) {
	result = FALSE;
    }

    R_CurrentExpr = topExp;
    R_HandlerStack = oldHStack;
    R_RestartStack = oldRStack;
    //R_ReturnedValue = oldRVal;
    R_Visible = oldvis;
    UNPROTECT(2);

    return result;
}



/*
  This is a simple interface for evaluating R expressions
  from C with a guarantee that one will return to the
  point in the code from which the call was made (if it does
  return at all).
  This uses R_TopleveExec to do this.  It is important
  in applications that embed R or wish to make general
  callbacks to R with error handling.

  It is currently hidden with a data structure definition
  and C routine visible only here. The R_tryEval() is the
  only visible aspect. This can be lifted into the header
  files if necessary. (DTL)

  R_tryEval is in Rinternals.h (so public), but not in the API.
 */
struct ProtectedEvalData {
    SEXP expression;
    GCStackRoot<> val;
    SEXP env;
};

static void
protectedEval(void *d)
{
    ProtectedEvalData *data = static_cast<ProtectedEvalData *>(d);
    SEXP env = R_GlobalEnv;
    if(data->env) {
	env = data->env;
    }
    data->val = Rf_eval(data->expression, env);
}

SEXP
R_tryEval(SEXP e, SEXP env, int *ErrorOccurred)
{
    Rboolean ok;
    ProtectedEvalData data;

    data.expression = e;
    data.val = nullptr;
    data.env = env;

    ok = R_ToplevelExec(protectedEval, &data);
    if (ErrorOccurred) {
	*ErrorOccurred = (ok == FALSE);
    }
    if (ok == FALSE)
	data.val = nullptr;

    return(data.val);
}

/* Temporary hack to suppress error message printing around a
   R_tryEval call for use in methods_list_dispatch.cpp; should be
   replaced once we have a way of establishing error handlers from C
   code (probably would want a calling handler if we want to allow
   user-defined calling handlers to enter a debugger, for
   example). LT */
SEXP R_tryEvalSilent(SEXP e, SEXP env, int *ErrorOccurred)
{
    SEXP val;
    Rboolean oldshow = Rboolean(R_ShowErrorMessages);
    R_ShowErrorMessages = FALSE;
    val = R_tryEval(e, env, ErrorOccurred);
    R_ShowErrorMessages = oldshow;
    return val;
}

SEXP R_ExecWithCleanup(SEXP (*fun)(void *), void *data,
		       void (*cleanfun)(void *), void *cleandata)
{
    SEXP result = nullptr;
    try {
	result = fun(data);
    } catch (CommandTerminated& ignored_error) { }

    cleanfun(cleandata);
    return result;
}
