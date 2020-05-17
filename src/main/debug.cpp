/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2015   The R Core Team.
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
#include "basedecl.h"
#include "rho/Closure.hpp"
#include "rho/FunctionBase.hpp"
#include "rho/FunctionContext.hpp"

using namespace rho;

SEXP attribute_hidden do_debug(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans = nullptr;

#define find_char_fun \
    if (Rf_isValidString(CAR(args))) {				\
	SEXP s;							\
	PROTECT(s = Rf_installTrChar(STRING_ELT(CAR(args), 0)));	\
	SETCAR(args, Rf_findFun(s, rho));				\
	UNPROTECT(1);						\
    }
    find_char_fun

    if (TYPEOF(CAR(args)) != CLOSXP &&
	TYPEOF(CAR(args)) != SPECIALSXP &&
	TYPEOF(CAR(args)) != BUILTINSXP)
	Rf_error(_("argument must be a function"));
    switch(PRIMVAL(op)) {
    case 0: // debug()
	SET_RDEBUG(CAR(args), TRUE);
	break;
    case 1: // undebug()
	if( RDEBUG(CAR(args)) != 1 )
	    Rf_warning("argument is not being debugged");
	SET_RDEBUG(CAR(args), FALSE);
	break;
    case 2: // isdebugged()
	ans = Rf_ScalarLogical(RDEBUG(CAR(args)));
	break;
    case 3: // debugonce()
	SET_RSTEP(CAR(args), 1);
	break;
    }
    return ans;
}

/* primitives .primTrace() and .primUntrace() */
SEXP attribute_hidden do_trace(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    find_char_fun

    if (TYPEOF(CAR(args)) != CLOSXP &&
	TYPEOF(CAR(args)) != SPECIALSXP &&
	TYPEOF(CAR(args)) != BUILTINSXP)
	    Rf_errorcall(call, _("argument must be a function"));

    switch(PRIMVAL(op)) {
    case 0:
	SET_RTRACE(CAR(args), 1);
	break;
    case 1:
	SET_RTRACE(CAR(args), 0);
	break;
    }
    return nullptr;
}


/* maintain global trace & debug state */

SEXP attribute_hidden do_traceOnOff(/*const*/ Expression* call, const BuiltInFunction* op, RObject* on_)
{
    SEXP onOff = on_;
    bool trace = op->variant() == 0;  // Otherwise it's debug.
    Rboolean prev = Rboolean(trace ? FunctionBase::tracingEnabled()
			     : Closure::debuggingEnabled());
    if(Rf_length(onOff) > 0) {
	Rboolean _new = Rboolean(Rf_asLogical(onOff));
	if(_new == TRUE || _new == FALSE) {
	    if (trace)
		FunctionBase::enableTracing(_new);
	    else
		Closure::enableDebugging(_new);
	}
	else
	    Rf_error(_("Value for '%s' must be TRUE or FALSE"),
		  trace ? "tracingState" : "debuggingState");
    }
    return Rf_ScalarLogical(prev);
}

// GUIs, packages, etc can query:
Rboolean attribute_hidden
R_current_trace_state() { return Rboolean(FunctionBase::tracingEnabled()); }

Rboolean attribute_hidden
R_current_debugging_state() { return Rboolean(Closure::debuggingEnabled()); }


/* memory tracing */
/* report when a traced object is duplicated */

#ifdef R_MEMORY_PROFILING
SEXP attribute_hidden do_tracemem(/*const*/ Expression* call, const BuiltInFunction* op, Environment* rho, RObject* const* args, int num_args, const PairList* tags)
{
    SEXP object;
    char buffer[21];

    object = args[0];
    if(object == nullptr)
	Rf_errorcall(call, _("cannot trace NULL"));

    if(TYPEOF(object) == ENVSXP || TYPEOF(object) == PROMSXP)
	Rf_warningcall(call,
		    _("'tracemem' is not useful for promise and environment objects"));
    if(TYPEOF(object) == EXTPTRSXP || TYPEOF(object) == WEAKREFSXP)
	Rf_warningcall(call,
		    _("'tracemem' is not useful for weak reference or external pointer objects"));

    object->setMemoryTracing(true);
    snprintf(buffer, 21, "<%p>", (void *) object);
    return Rf_mkString(buffer);
}


SEXP attribute_hidden do_untracemem(/*const*/ Expression* call, const BuiltInFunction* op, Environment* rho, RObject* const* args, int num_args, const PairList* tags)
{
    SEXP object;

    object=args[0];
    object->setMemoryTracing(false);
    return nullptr;
}

#else

SEXP attribute_hidden do_tracemem(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_)
{
    Rf_errorcall(call, _("R was not compiled with support for memory profiling"));
}

SEXP attribute_hidden do_untracemem(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_)
{
    Rf_errorcall(call, _("R was not compiled with support for memory profiling"));
}

#endif /* R_MEMORY_PROFILING */

#ifdef R_MEMORY_PROFILING
static void memtrace_stack_dump(void)
{
    Evaluator::Context *cptr;

    for (cptr = Evaluator::Context::innermost();
	 cptr; cptr = cptr->nextOut()) {
	Evaluator::Context::Type type = cptr->type();
	if (type == Evaluator::Context::FUNCTION
	    || type == Evaluator::Context::CLOSURE) {
	    FunctionContext* fctxt = static_cast<FunctionContext*>(cptr);
	    SEXP fun = fctxt->call()->car();
	    Rprintf("%s ",
		    TYPEOF(fun) == SYMSXP ? Rf_translateChar(PRINTNAME(fun)) :
		    "<Anonymous>");
	}
    }
    Rprintf("\n");
}

void RObject::traceMemory(const RObject* src1, const RObject* src2,
			  const RObject* src3)
{
    setMemoryTracing(true);
    Rprintf("tracemem[");
    bool needs_comma = false;
    if (src1->memoryTraced()) {
	Rprintf("%p", src1);
	needs_comma = true;
    }
    if (src2 && src2->memoryTraced()) {
	if (needs_comma)
	    Rprintf(", ");
	Rprintf("%p", src2);
	needs_comma = true;
    }
    if (src3 && src3->memoryTraced()) {
	if (needs_comma)
	    Rprintf(", ");
	Rprintf("%p", src3);
    }
    Rprintf(" -> %p]: ", this);
    memtrace_stack_dump();
}

#endif /* R_MEMORY_PROFILING */

SEXP do_retracemem(SEXP call, SEXP op, SEXP arg, SEXP rho)
{
#ifdef R_MEMORY_PROFILING
    SEXP object, previous, ans;
    char buffer[21];

    static GCRoot<ArgMatcher> matcher = new ArgMatcher({ "x", "previous" });
    matcher->match(&arglist, { &object, &previous });
    if (object == R_MissingArg)
	object = nullptr;
    if (previous == R_MissingArg)
	previous = nullptr;

    if(!Rf_isNull(previous) && (!Rf_isString(previous) || LENGTH(previous) != 1))
	    Rf_errorcall(call, _("invalid '%s' argument"), "previous");

    if (object->memoryTraced()){
	snprintf(buffer, 21, "<%p>", (void *) object);
	ans = Rf_mkString(buffer);
    } else {
	R_Visible = FALSE;
	ans = nullptr;
    }

    if (previous != nullptr){
	object->setMemoryTracing(true);
	if (R_current_trace_state()) {
	    /* FIXME: previous will have <0x....> whereas other values are
	       without the < > */
	    Rprintf("tracemem[%s -> %p]: ",
		    Rf_translateChar(STRING_ELT(previous, 0)), (void *) object);
	    memtrace_stack_dump();
	}
    }
    return ans;
#else
    R_Visible = FALSE; /* for consistency with other case */
    return nullptr;
#endif
}
