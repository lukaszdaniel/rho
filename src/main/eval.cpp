/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2017	The R Core Team.
 *  Copyright (C) 1995, 1996	Robert Gentleman and Ross Ihaka
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

/** @file eval.cpp
 *
 * General evaluation of expressions, including implementation of R flow
 * control constructs, and R profiling.
 */


#define R_NO_REMAP

// For debugging:
#include <iostream>

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
#include <Defn.h>
#include <Localization.h>
#include <Internal.h>
#include <Rinterface.h>
#include <Fileio.h>
#include "arithmetic.h"
#include "basedecl.h"

#include <rho/ArgList.hpp>
#include <rho/BailoutContext.hpp>
#include <rho/BuiltInFunction.hpp>
#include <rho/Closure.hpp>
#include <rho/ClosureContext.hpp>
#include <rho/DottedArgs.hpp>
#include <rho/ExpressionVector.hpp>
#include <rho/GCStackFrameBoundary.hpp>
#include <rho/Frame.hpp>
#include <rho/LoopBailout.hpp>
#include <rho/LoopException.hpp>
#include <rho/Promise.hpp>
#include <rho/ProvenanceTracker.hpp>
#include <rho/ReturnBailout.hpp>
#include <rho/ReturnException.hpp>
#include <rho/S3Launcher.hpp>

using namespace std;
using namespace rho;

#ifdef R_PROFILING

/* BDR 2000-07-15
   Profiling is now controlled by the R function Rprof(), and should
   have negligible cost when not enabled.
*/

/* A simple mechanism for profiling R code.  When R_PROFILING is
   enabled, eval will write out the call stack every PROFSAMPLE
   microseconds using the SIGPROF handler triggered by timer signals
   from the ITIMER_PROF timer.  Since this is the same timer used by C
   profiling, the two cannot be used together.  Output is written to
   the file PROFOUTNAME.  This is a plain text file.  The first line
   of the file contains the value of PROFSAMPLE.  The remaining lines
   each give the call stack found at a sampling point with the inner
   most function first.

   To enable profiling, recompile eval.cpp with R_PROFILING defined.  It
   would be possible to selectively turn profiling on and off from R
   and to specify the file name from R as well, but for now I won't
   bother.

   The stack is traced by walking back along the context stack, just
   like the traceback creation in jump_to_toplevel.  One drawback of
   this approach is that it does not show BUILTIN's since they don't
   get a context.  With recent changes to pos.to.env it seems possible
   to insert a context around BUILTIN calls to that they show up in
   the trace.  Since there is a cost in establishing these contexts,
   they are only inserted when profiling is enabled. [BDR: we have since
   also added contexts for the BUILTIN calls to foreign code.]

   One possible advantage of not tracing BUILTIN's is that then
   profiling adds no cost when the timer is turned off.  This would be
   useful if we want to allow profiling to be turned on and off from
   within R.

   One thing that makes interpreting profiling output tricky is lazy
   evaluation.  When an expression f(g(x)) is profiled, lazy
   evaluation will cause g to be called inside the call to f, so it
   will appear as if g is called by f.

   L. T.  */

#ifdef Win32
# define WIN32_LEAN_AND_MEAN 1
# include <windows.h>		/* for CreateEvent, SetEvent */
# include <process.h>		/* for _beginthread, _endthread */
#else
# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# endif
# include <signal.h>
#endif /* not Win32 */

static FILE *R_ProfileOutfile = nullptr;
static int R_Mem_Profiling=0;
extern void get_current_mem(unsigned long *,unsigned long *,unsigned long *); /* in memory.cpp */
extern unsigned long get_duplicate_counter(void);  /* in duplicate.cpp */
extern void reset_duplicate_counter(void);         /* in duplicate.cpp */
static int R_GC_Profiling = 0;                     /* indicates GC profiling */
static int R_Line_Profiling = 0;                   /* indicates line profiling, and also counts the filenames seen (+1) */
static char **R_Srcfiles;			   /* an array of pointers into the filename buffer */
static size_t R_Srcfile_bufcount;                  /* how big is the array above? */
static GCRoot<> R_Srcfiles_buffer = nullptr;              /* a big RAWSXP to use as a buffer for filenames and pointers to them */
static int R_Profiling_Error;		   /* record errors here */

#ifdef Win32
HANDLE MainThread;
HANDLE ProfileEvent;
#endif /* Win32 */

/* Careful here!  These functions are called asynchronously, maybe in the middle of GC,
   so don't do any allocations */

/* This does a linear search through the previously recorded filenames.  If
   this one is new, we try to add it.  FIXME:  if there are eventually
   too many files for an efficient linear search, do hashing. */

static int getFilenum(const char* filename) {
    int fnum;

    for (fnum = 0; fnum < R_Line_Profiling-1
		   && strcmp(filename, R_Srcfiles[fnum]); fnum++);

    if (fnum == R_Line_Profiling-1) {
	size_t len = strlen(filename);
	if (fnum >= int(R_Srcfile_bufcount)) { /* too many files */
	    R_Profiling_Error = 1;
	    return 0;
	}
	if (R_Srcfiles[fnum] - reinterpret_cast<char*>(RAW(R_Srcfiles_buffer)) + len + 1 > (long unsigned int) Rf_length(R_Srcfiles_buffer)) {
	      /* out of space in the buffer */
	    R_Profiling_Error = 2;
	    return 0;
	}
	strcpy(R_Srcfiles[fnum], filename);
	R_Srcfiles[fnum+1] = R_Srcfiles[fnum] + len + 1;
	*(R_Srcfiles[fnum+1]) = '\0';
	R_Line_Profiling++;
    }

    return fnum + 1;
}

/* These, together with sprintf/strcat, are not safe -- we should be
   using snprintf and such and computing needed sizes, but these
   settings are better than what we had. LT */

constexpr size_t PROFBUFSIZ = 10500;
constexpr size_t PROFITEMMAX = 500;
constexpr size_t PROFLINEMAX = (PROFBUFSIZ - PROFITEMMAX);

/* It would also be better to flush the buffer when it gets full,
   even if the line isn't complete. But this isn't possible if we rely
   on writing all line profiling files first.  With these sizes
   hitting the limit is fairly unlikely, but if we do then the output
   file is wrong. Maybe writing an overflow marker of some sort would
   be better.  LT */

static void lineprof(char* buf, SEXP srcref)
{
    size_t len;
    if (srcref && !Rf_isNull(srcref) && (len = strlen(buf)) < PROFLINEMAX) {
	int fnum, line = Rf_asInteger(srcref);
	SEXP srcfile = Rf_getAttrib(srcref, Symbols::SrcfileSymbol);
	const char *filename;

	if (!srcfile || TYPEOF(srcfile) != ENVSXP) return;
	srcfile = Rf_findVar(Rf_install("filename"), srcfile);
	if (TYPEOF(srcfile) != STRSXP || !Rf_length(srcfile)) return;
	filename = R_CHAR(STRING_ELT(srcfile, 0));

	if ((fnum = getFilenum(filename)))
	    snprintf(buf+len, PROFBUFSIZ - len, "%d#%d ", fnum, line);
    }
}

/* FIXME: This should be done wih a proper configure test, also making
   sure that the pthreads library is linked in. LT */
#ifndef Win32
#if (defined(__APPLE__) || defined(_REENTRANT) || defined(HAVE_OPENMP)) && \
     ! defined(HAVE_PTHREAD)
# define HAVE_PTHREAD
#endif
#ifdef HAVE_PTHREAD
# include <pthread.h>
static pthread_t R_profiled_thread;
# endif
#endif

static void doprof(int sig)  /* sig is ignored in Windows */
{
    char buf[PROFBUFSIZ];
    unsigned long bigv, smallv, nodes;
    size_t len;
    int prevnum = R_Line_Profiling;

    buf[0] = '\0';

#ifdef Win32
    SuspendThread(MainThread);
#elif defined(HAVE_PTHREAD)
    if (! pthread_equal(pthread_self(), R_profiled_thread)) {
	pthread_kill(R_profiled_thread, sig);
	return;
    }
#endif /* Win32 */

    if (R_Mem_Profiling){
	    get_current_mem(&smallv, &bigv, &nodes);
	    if((len = strlen(buf)) < PROFLINEMAX)
		snprintf(buf+len, PROFBUFSIZ - len,
			 ":%ld:%ld:%ld:%ld:", smallv, bigv,
			 nodes, get_duplicate_counter());
	    reset_duplicate_counter();
    }

// R_gc_running() not (yet) implemented in rho:
#if 0
    if (R_GC_Profiling && R_gc_running())
	strcat(buf, "\"<GC>\" ");
#endif

    if (R_Line_Profiling)
	lineprof(buf, R_Srcref);

// rho FIXME: not yet adapted for rho:
#if 0
    for (Evaluator::Context* cptr = Evaluator::Context::innermost();
	 cptr; cptr = cptr->nextOut()) {
	if (TYPEOF(cptr->call) == LANGSXP) {
	    SEXP fun = CAR(cptr->call);
	    if(strlen(buf) < PROFLINEMAX) {
		strcat(buf, "\"");

		char itembuf[PROFITEMMAX];

		if (TYPEOF(fun) == SYMSXP) {
		    snprintf(itembuf, PROFITEMMAX-1, "%s", R_CHAR(PRINTNAME(fun)));

		} else if ((CAR(fun) == Symbols::DoubleColonSymbol ||
			    CAR(fun) == Symbols::TripleColonSymbol ||
			    CAR(fun) == Symbols::DollarSymbol) &&
			   TYPEOF(CADR(fun)) == SYMSXP &&
			   TYPEOF(CADDR(fun)) == SYMSXP) {
		    /* Function accessed via ::, :::, or $. Both args must be
		       symbols. It is possible to use strings with these
		       functions, as in "base"::"list", but that's a very rare
		       case so we won't bother handling it. */
		    snprintf(itembuf, PROFITEMMAX-1, "%s%s%s",
			     R_CHAR(PRINTNAME(CADR(fun))),
			     R_CHAR(PRINTNAME(CAR(fun))),
			     R_CHAR(PRINTNAME(CADDR(fun))));

		} else if (CAR(fun) == Symbols::Bracket2Symbol &&
			   TYPEOF(CADR(fun)) == SYMSXP &&
			   ((TYPEOF(CADDR(fun)) == SYMSXP ||
			     TYPEOF(CADDR(fun)) == STRSXP ||
			     TYPEOF(CADDR(fun)) == INTSXP ||
			     TYPEOF(CADDR(fun)) == REALSXP) &&
			    Rf_length(CADDR(fun)) > 0)) {
		    /* Function accessed via [[. The first arg must be a symbol
		       and the second can be a symbol, string, integer, or
		       real. */
		    SEXP arg1 = CADR(fun);
		    SEXP arg2 = CADDR(fun);
		    char arg2buf[PROFITEMMAX-5];

		    if (TYPEOF(arg2) == SYMSXP) {
			snprintf(arg2buf, PROFITEMMAX-6, "%s", R_CHAR(PRINTNAME(arg2)));

		    } else if (TYPEOF(arg2) == STRSXP) {
			snprintf(arg2buf, PROFITEMMAX-6, "\"%s\"", R_CHAR(STRING_ELT(arg2, 0)));

		    } else if (TYPEOF(arg2) == INTSXP) {
			snprintf(arg2buf, PROFITEMMAX-6, "%d", INTEGER(arg2)[0]);

		    } else if (TYPEOF(arg2) == REALSXP) {
			snprintf(arg2buf, PROFITEMMAX-6, "%.0f", REAL(arg2)[0]);

		    } else {
			/* Shouldn't get here, but just in case. */
			arg2buf[0] = '\0';
		    }

		    snprintf(itembuf, PROFITEMMAX-1, "%s[[%s]]",
			     R_CHAR(PRINTNAME(arg1)),
			     arg2buf);

		} else {
		    sprintf(itembuf, "<Anonymous>");
		}

		strcat(buf, itembuf);
		strcat(buf, "\" ");
		if (R_Line_Profiling)
		    lineprof(buf, cptr->srcref);
	    }
	}
    }
#endif

    /* I believe it would be slightly safer to place this _after_ the
       next two bits, along with the signal() call. LT */
#ifdef Win32
    ResumeThread(MainThread);
#endif /* Win32 */

    for (int i = prevnum; i < R_Line_Profiling; i++)
	fprintf(R_ProfileOutfile, "#File %d: %s\n", i, R_Srcfiles[i-1]);

    if(strlen(buf))
	fprintf(R_ProfileOutfile, "%s\n", buf);

#ifndef Win32
    signal(SIGPROF, doprof);
#endif /* not Win32 */

}

#ifdef Win32
/* Profiling thread main function */
static void __cdecl ProfileThread(void *pwait)
{
    int wait = *((int *)pwait);

    SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_HIGHEST);
    while(WaitForSingleObject(ProfileEvent, wait) != WAIT_OBJECT_0) {
	doprof(0);
    }
}
#else /* not Win32 */
static void doprof_null(int sig)
{
    signal(SIGPROF, doprof_null);
}
#endif /* not Win32 */


static void R_EndProfiling(void)
{
#ifdef Win32
    SetEvent(ProfileEvent);
    CloseHandle(MainThread);
#else /* not Win32 */
    struct itimerval itv;

    itv.it_interval.tv_sec = 0;
    itv.it_interval.tv_usec = 0;
    itv.it_value.tv_sec = 0;
    itv.it_value.tv_usec = 0;
    setitimer(ITIMER_PROF, &itv, nullptr);
    signal(SIGPROF, doprof_null);

#endif /* not Win32 */
    if(R_ProfileOutfile) fclose(R_ProfileOutfile);
    R_ProfileOutfile = nullptr;
    Evaluator::enableProfiling(false);
    if (R_Srcfiles_buffer) {
	R_ReleaseObject(R_Srcfiles_buffer);
	R_Srcfiles_buffer = nullptr;
    }
    if (R_Profiling_Error)
	Rf_warning(_("source files skipped by Rprof; please increase '%s'"),
		R_Profiling_Error == 1 ? "numfiles" : "bufsize");
}

static void R_InitProfiling(SEXP filename, int append, double dinterval,
			    int mem_profiling, int gc_profiling,
			    int line_profiling, int numfiles, int bufsize)
{
#ifndef Win32
    struct itimerval itv;
#else
    int wait;
    HANDLE Proc = GetCurrentProcess();
#endif
    int interval;

    interval = int(1e6 * dinterval + 0.5);
    if(R_ProfileOutfile != nullptr) R_EndProfiling();
    R_ProfileOutfile = RC_fopen(filename, append ? "a" : "w", TRUE);
    if (R_ProfileOutfile == nullptr)
	Rf_error(_("Rprof: cannot open profile file '%s'"),
	      Rf_translateChar(filename));
    if(mem_profiling)
	fprintf(R_ProfileOutfile, "memory profiling: ");
    if(gc_profiling)
	fprintf(R_ProfileOutfile, "GC profiling: ");
    if(line_profiling)
	fprintf(R_ProfileOutfile, "line profiling: ");
    fprintf(R_ProfileOutfile, "sample.interval=%d\n", interval);

    R_Mem_Profiling=mem_profiling;
    if (mem_profiling)
	reset_duplicate_counter();

    R_Profiling_Error = 0;
    R_Line_Profiling = line_profiling;
    R_GC_Profiling = gc_profiling;
    if (line_profiling) {
	/* Allocate a big RAW vector to use as a buffer.  The first len1 bytes are an array of pointers
	   to strings; the actual strings are stored in the second len2 bytes. */
	R_Srcfile_bufcount = numfiles;
	size_t len1 = R_Srcfile_bufcount*sizeof(char *), len2 = bufsize;
	R_PreserveObject( R_Srcfiles_buffer = Rf_allocVector(RAWSXP, len1 + len2) );
 //	memset(RAW(R_Srcfiles_buffer), 0, len1+len2);
	R_Srcfiles = reinterpret_cast<char **>(RAW(R_Srcfiles_buffer));
	R_Srcfiles[0] = reinterpret_cast<char *>(RAW(R_Srcfiles_buffer)) + len1;
	*(R_Srcfiles[0]) = '\0';
    }

#ifdef Win32
    /* need to duplicate to make a real handle */
    DuplicateHandle(Proc, GetCurrentThread(), Proc, &MainThread,
		    0, FALSE, DUPLICATE_SAME_ACCESS);
    wait = interval/1000;
    if(!(ProfileEvent = CreateEvent(nullptr, FALSE, FALSE, nullptr)) ||
       (_beginthread(ProfileThread, 0, &wait) == -1))
	R_Suicide("unable to create profiling thread");
    Sleep(wait/2); /* suspend this thread to ensure that the other one starts */
#else /* not Win32 */
#ifdef HAVE_PTHREAD
    R_profiled_thread = pthread_self();
#endif

    signal(SIGPROF, doprof);

    itv.it_interval.tv_sec = 0;
    itv.it_interval.tv_usec = interval;
    itv.it_value.tv_sec = 0;
    itv.it_value.tv_usec = interval;
    if (setitimer(ITIMER_PROF, &itv, nullptr) == -1)
	R_Suicide("setting profile timer failed");
#endif /* not Win32 */
    Evaluator::enableProfiling(true);
}

SEXP do_Rprof(SEXP args)
{
    SEXP filename;
    int append_mode, mem_profiling, gc_profiling, line_profiling;
    double dinterval;
    int numfiles, bufsize;

    if (!Rf_isString(filename = CAR(args)) || (LENGTH(filename)) != 1)
	Rf_error(_("invalid '%s' argument"), "filename");
					      args = CDR(args);
    append_mode = Rf_asLogical(CAR(args));       args = CDR(args);
    dinterval = Rf_asReal(CAR(args));            args = CDR(args);
    mem_profiling = Rf_asLogical(CAR(args));     args = CDR(args);
    gc_profiling = Rf_asLogical(CAR(args));      args = CDR(args);
    line_profiling = Rf_asLogical(CAR(args));    args = CDR(args);
    numfiles = Rf_asInteger(CAR(args));	      args = CDR(args);
    if (numfiles < 0)
	Rf_error(_("invalid '%s' argument"), "numfiles");
    bufsize = Rf_asInteger(CAR(args));
    if (bufsize < 0)
	Rf_error(_("invalid '%s' argument"), "bufsize");

    filename = STRING_ELT(filename, 0);
    if (LENGTH(filename))
	R_InitProfiling(filename, append_mode, dinterval, mem_profiling,
			gc_profiling, line_profiling, numfiles, bufsize);
    else
	R_EndProfiling();
    return nullptr;
}
#else /* not R_PROFILING */
SEXP do_Rprof(SEXP args)
{
    Rf_error(_("R profiling is not available on this system"));
    return nullptr;		/* -Wall */
}
#endif /* not R_PROFILING */

/* NEEDED: A fixup is needed in browser, because it can trap errors,
 *	and currently does not reset the limit to the right value. */

static SEXP forcePromise(SEXP e)
{
    Promise* prom = SEXP_downcast<Promise*>(e);
    return prom->force();
}

HIDDEN void Rf_SrcrefPrompt(const char * prefix, SEXP srcref)
{
    /* If we have a valid srcref, use it */
    if (srcref && srcref != nullptr) {
	if (TYPEOF(srcref) == VECSXP) srcref = VECTOR_ELT(srcref, 0);
	SEXP srcfile = Rf_getAttrib(srcref, Symbols::SrcfileSymbol);
	if (TYPEOF(srcfile) == ENVSXP) {
	    SEXP filename = Rf_findVar(Rf_install("filename"), srcfile);
	    if (Rf_isString(filename) && Rf_length(filename)) {
		Rprintf(_("%s at %s#%d: "), prefix, R_CHAR(STRING_ELT(filename, 0)), 
			                    Rf_asInteger(srcref));
		return;
	    }
	}
    }
    /* default: */
    Rprintf("%s: ", prefix);
}

/* this function gets the srcref attribute from a statement block,
   and confirms it's in the expected format */

R_INLINE static SEXP getBlockSrcrefs(SEXP call)
{
    SEXP srcrefs = Rf_getAttrib(call, Symbols::SrcrefSymbol);
    if (TYPEOF(srcrefs) == VECSXP) return srcrefs;
    return nullptr;
}

/* this function extracts one srcref, and confirms the format */
/* It assumes srcrefs has already been validated to be a VECSXP or NULL */

R_INLINE static SEXP getSrcref(SEXP srcrefs, int ind)
{
    SEXP result;
    if (!Rf_isNull(srcrefs)
	&& Rf_length(srcrefs) > ind
	&& !Rf_isNull(result = VECTOR_ELT(srcrefs, ind))
	&& TYPEOF(result) == INTSXP
	&& Rf_length(result) >= 6)
	return result;
    return nullptr;
}

/* There's another copy of this in main.cpp */
static void PrintCall(SEXP call, SEXP rho)
{
    int old_bl = R_BrowseLines,
        blines = Rf_asInteger(Rf_GetOption1(Rf_install("deparse.max.lines")));
    if(blines != R_NaInt && blines > 0)
	R_BrowseLines = blines;
    Rf_PrintValueRec(call, rho);
    R_BrowseLines = old_bl;
}

void Closure::DebugScope::startDebugging() const
{
    const ClosureContext* ctxt = R_GlobalContext();
    const Expression* call = ctxt->call();
    Environment* working_env = ctxt->workingEnvironment();
    working_env->setSingleStepping(true);
    Rprintf("debugging in: ");
    PrintCall(const_cast<Expression*>(call), working_env);

    Rprintf("debug: ");
    Rf_PrintValue(m_closure->m_body);
    do_browser(nullptr, nullptr, nullptr, working_env);
}

void Closure::DebugScope::endDebugging() const
{
    const ClosureContext* ctxt = R_GlobalContext();
    try {
	Rprintf("exiting from: ");
	PrintCall(const_cast<Expression*>(ctxt->call()), nullptr);
    }
    // Don't allow exceptions to escape destructor:
    catch (...) {}
}


SEXP R_forceAndCall(SEXP e, int n, SEXP rho)
{
    Expression* call = SEXP_downcast<Expression*>(e);
    Environment* env = SEXP_downcast<Environment*>(rho);

    SEXP fun, result;
    if (TYPEOF(CAR(e)) == SYMSXP)
       /* This will throw an error if the function is not found */
       PROTECT(fun = Rf_findFun(CAR(e), rho));
    else
       PROTECT(fun = Rf_eval(CAR(e), rho));

    ArgList arglist(call->tail(), ArgList::RAW);

    BuiltInFunction* builtin = dynamic_cast<BuiltInFunction*>(fun);
    if (builtin) {
	result = call->evaluateFunctionCall(builtin, env, arglist);
    } else if (TYPEOF(fun) == CLOSXP) {
	Closure* closure = SEXP_downcast<Closure*>(fun);

	arglist.wrapInPromises(env);

	// Force the promises.
	int i = 0;
	auto args = arglist.getArgs();
	for (auto cell = args.begin(); i < n && cell != args.end(); ++cell, ++i)
	{
	    SEXP p = cell->car();
	    if (TYPEOF(p) == PROMSXP)
		Rf_eval(p, rho);
	    else if (p == R_MissingArg)
		Rf_errorcall(e, _("argument %d is empty"), i + 1);
	    else
		Rf_error("something weird happened");
	}
	result = call->evaluateFunctionCall(closure, env, arglist);
    }
    else {
       Rf_error(_("attempt to apply non-function"));
    }

    UNPROTECT(1);
    return result;
}

HIDDEN SEXP do_forceAndCall(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int n = Rf_asInteger(Rf_eval(CADR(call), rho));
    SEXP e = CDDR(call);

    /* this would not be needed if CDDR(call) was a LANGSXP */
    Expression* expr = new Expression(CAR(e), SEXP_downcast<PairList*>(CDR(e)));
    SEXP val = R_forceAndCall(expr, n, rho);
    UNPROTECT(1);
    return val;
}

/* **** FIXME: Temporary code to execute S4 methods in a way that
   **** preserves lexical scope. */

/* called from methods_list_dispatch.cpp */
SEXP R_execMethod(SEXP op, SEXP rho)
{
    Closure* func = SEXP_downcast<Closure*>(op);
    Environment* callenv = SEXP_downcast<Environment*>(rho);
    const Frame* fromf = callenv->frame();

    /* Find the calling context. */
    ClosureContext* cptr = R_GlobalContext();

    /* The calling environment should either be the environment of the
       generic, rho, or the environment of the caller of the generic,
       the current sysparent. */
    Environment* callerenv = cptr->callEnvironment(); /* or rho? */

    // create a new environment frame enclosed by the lexical
    // environment of the method
    const ArgList& args = cptr->promiseArgs();
    GCStackRoot<Environment> newrho(func->createExecutionEnv(args));
    Frame* newframe = newrho->frame();

    // Propagate bindings of the formal arguments of the generic to
    // newrho, but replace defaulted arguments with those appropriate
    // to the method:
    func->matcher()->propagateFormalBindings(callenv, newrho);

    /* copy the bindings of the special dispatch variables in the top
       frame of the generic call to the new frame */
    {
	static const Symbol* syms[]
	    = {DotdefinedSymbol, DotMethodSymbol, DottargetSymbol, nullptr};
	for (const Symbol** symp = syms; *symp; ++symp) {
	    newframe->importBinding(fromf->binding(*symp));
	}
    }

    /* copy the bindings for .Generic and .Methods.  We know (I think)
       that they are in the second frame, so we could use that. */
    {
	static const Symbol* syms[]
	    = {DotGenericSymbol, DotMethodsSymbol, nullptr};
	for (const Symbol** symp = syms; *symp; ++symp) {
	    const Frame::Binding* frombdg = callenv->findBinding(*symp);
	    newframe->importBinding(frombdg);
	}
    }

    // Set up context and perform evaluation.  Note that ans needs to
    // be protected in case the destructor of ClosureContext executes
    // an on.exit function.
    GCStackRoot<> ans;
    {
	ClosureContext ctxt(cptr->call(), callerenv, func, newrho);
	ans = func->execute(newrho);
    }
    return ans;
}

static SEXP EnsureLocal(SEXP symbol, SEXP rho)
{
    GCStackRoot<> vl;

    if ((vl = Rf_findVarInFrame3(rho, symbol, TRUE)) != R_UnboundValue) {
	vl = Rf_eval(symbol, rho);	/* for promises */
	if(NAMED(vl) == 2) {
	    vl = Rf_duplicate(vl);
	    Rf_defineVar(symbol, vl, rho);
	    INCREMENT_NAMED(vl);
	}
	return vl;
    }

    vl = Rf_eval(symbol, ENCLOS(rho));
    if (vl == R_UnboundValue)
	Rf_error(_("object '%s' not found"), R_CHAR(PRINTNAME(symbol)));

    vl = Rf_duplicate(vl);
    Rf_defineVar(symbol, vl, rho);
	INCREMENT_NAMED(vl);
    return vl;
}


/* Note: If val is a language object it must be protected */
/* to prevent evaluation.  As an example consider */
/* e <- quote(f(x=1,y=2); names(e) <- c("","a","b") */

static SEXP replaceCall(SEXP fun, SEXP val, SEXP args, SEXP rhs)
{
    static Symbol* valuesym = Symbol::obtain("value");
    SEXP tmp, ptmp;
    PROTECT(fun);
    PROTECT(args);
    PROTECT(rhs);
    PROTECT(val);
    GCStackRoot<PairList> tl(PairList::make(Rf_length(args) + 2));
    ptmp = tmp = new Expression(nullptr, tl);
    UNPROTECT(4);
    SETCAR(ptmp, fun); ptmp = CDR(ptmp);
    SETCAR(ptmp, val); ptmp = CDR(ptmp);
    while(args != nullptr) {
	SETCAR(ptmp, CAR(args));
	SET_TAG(ptmp, TAG(args));
	ptmp = CDR(ptmp);
	args = CDR(args);
    }
    SETCAR(ptmp, rhs);
    SET_TAG(ptmp, valuesym);
    return tmp;
}


static SEXP assignCall(SEXP op, SEXP symbol, SEXP fun,
		       SEXP val, SEXP args, SEXP rhs)
{
    PROTECT(op);
    PROTECT(symbol);
    val = replaceCall(fun, val, args, rhs);
    UNPROTECT(2);
    return Rf_lang3(op, symbol, val);
}


/* rho only needed for _R_CHECK_LENGTH_1_CONDITION_=package:name */
Rboolean asLogicalNoNA(SEXP s, SEXP call, SEXP rho)
{
    int cond = R_NaLog;

    /* handle most common special case directly */
    if (IS_SCALAR(s, LGLSXP)) {
	cond = SCALAR_LVAL(s);
	if (cond != R_NaLog)
	    return Rboolean(cond);
    }
    else if (IS_SCALAR(s, INTSXP)) {
	int val = SCALAR_IVAL(s);
	if (val != R_NaInt)
	    return Rboolean(val != 0);
    }

    int len = Rf_length(s);
    if (len > 1)
    {
	GCStackRoot<> gc_protect(s);
	char *check = getenv("_R_CHECK_LENGTH_1_CONDITION_");
	const void *vmax = vmaxget();
	Rboolean err = Rboolean(check && Rf_StringTrue(check)); /* warn by default */
	if (!err && check) {
	    /* err when the condition is evaluated in given package */
	    const char *pprefix  = "package:";
	    size_t lprefix = strlen(pprefix);
	    if (!strncmp(pprefix, check, lprefix)) {
		/* check starts with "package:" */
		SEXP spkg = nullptr;
		for(; spkg == nullptr && rho != R_EmptyEnv; rho = ENCLOS(rho))
		    if (R_IsPackageEnv(rho))
			spkg = R_PackageEnvName(rho);
		    else if (R_IsNamespaceEnv(rho))
			spkg = R_NamespaceEnvSpec(rho);
		if (spkg != nullptr) {
		    const char *pkgname = Rf_translateChar(STRING_ELT(spkg, 0));
		    if (!strcmp(check + lprefix, pkgname))
			err = TRUE;
		    if (!strcmp(check + lprefix, "_R_CHECK_PACKAGE_NAME_")) {
			/* package name specified in _R_CHECK_PACKAGE_NAME */
			const char *envpname = getenv("_R_CHECK_PACKAGE_NAME_");
			if (envpname && !strcmp(envpname, pkgname))
			    err = TRUE;
		    }
		}
	    }
	}
	if (err)
	    Rf_errorcall(call, _("the condition has length > 1"));
        else
	    Rf_warningcall(call,
		    _("the condition has length > 1 and only the first element will be used"));
	vmaxset(vmax);
    }
    if (len> 0) {
	/* inline common cases for efficiency */
	switch(TYPEOF(s)) {
	case LGLSXP:
	    cond = LOGICAL(s)[0];
	    break;
	case INTSXP:
	    cond = INTEGER(s)[0]; /* relies on R_NaInt == R_NaLog */
	    break;
	default:
	    cond = Rf_asLogical(s);
	}
    }

    if (cond == R_NaLog) {
	char *msg = len ? (Rf_isLogical(s) ?
				 _("missing value where TRUE/FALSE needed") :
				 _("argument is not interpretable as logical")) :
	    _("argument is of length zero");
	Rf_errorcall(call, msg);
    }
    return Rboolean(cond);
}


namespace {
    inline int BodyHasBraces(SEXP body)
    {
	return (Rf_isLanguage(body) && CAR(body) == Symbols::BraceSymbol) ? 1 : 0;
    }

    inline void DO_LOOP_RDEBUG(SEXP call, SEXP op, SEXP args, SEXP rho, int bgn)
    {
	if (bgn && ENV_RDEBUG(rho)) {
	    Rf_SrcrefPrompt("debug", R_Srcref);
	    Rf_PrintValue(CAR(args));
	    do_browser(nullptr, nullptr, nullptr, rho);
	}
    }

    /* Allocate space for the loop variable value the first time through
       (when v == nullptr) and when the value has been assigned to
       another variable (NAMED(v) == 2). This should be safe and avoid
       allocation in many cases. */
    inline RObject* ALLOC_LOOP_VAR(RObject* v, SEXPTYPE val_type)
    {
	if (!v || NAMED(v) == 2)
	    v = Rf_allocVector(val_type, 1);
	return v;
    }

    RObject* propagateBailout(RObject* bailout)
    {
	Evaluator::Context* callctxt
	    = Evaluator::Context::innermost()->nextOut();
	if (!callctxt || callctxt->type() != Evaluator::Context::BAILOUT) {
	    static_cast<Bailout*>(bailout)->throwException();
	}
	return bailout;
    }
}

HIDDEN SEXP do_if(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP Cond, Stmt=nullptr;
    int vis=0;

    PROTECT(Cond = Rf_eval(CAR(args), rho));
    if (asLogicalNoNA(Cond, call, rho))
	Stmt = CAR(CDR(args));
    else {
	if (Rf_length(args) > 2) 
	   Stmt = CAR(CDR(CDR(args)));
	else
	   vis = 1;
    }
    if( ENV_RDEBUG(rho) && !BodyHasBraces(Stmt)) {
	Rf_SrcrefPrompt("debug", R_Srcref);
	Rf_PrintValue(Stmt);
	do_browser(nullptr, nullptr, nullptr, rho);
    }
    UNPROTECT(1);
    if( vis ) {
	R_Visible = FALSE; /* case of no 'else' so return invisible NULL */
	return Stmt;
    }

    {
	RObject* ans;
	{
	    BailoutContext bcntxt;
	    ans = Rf_eval(Stmt, rho);
	}
	if (ans && ans->sexptype() == BAILSXP) {
	    return propagateBailout(ans);
	}
	return ans;
    }
}

HIDDEN SEXP do_for_impl(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    GCStackRoot<> argsrt(args), rhort(rho);

    /* Need to declare volatile variables whose values are relied on
       after for_next or for_break longjmps and might change between
       the setjmp and longjmp calls. Theoretically this does not
       include n and bgn, but gcc -O2 -Wclobbered warns about these so
       to be safe we declare them volatile as well. */
    volatile int i = 0, n, bgn;
    Rboolean dbg;
    SEXPTYPE val_type;
    GCStackRoot<> ans, v, val;
    SEXP sym, body;

    sym = CAR(args);
    val = CADR(args);
    body = CADDR(args);

    if ( !Rf_isSymbol(sym) ) Rf_errorcall(call, _("non-symbol loop variable"));

    dbg = ENV_RDEBUG(rho);
    /* rho FIXME
    if (R_jit_enabled > 2 && !dbg && ! R_PendingPromises) {
	R_compileAndExecute(call, rho);
	return nullptr;
    }
    */

    val = Rf_eval(val, rho);
    Rf_defineVar(sym, nullptr, rho);

    /* deal with the case where we are iterating over a factor
       we need to coerce to character - then iterate */

    if( Rf_inherits(val, "factor") ) {
	ans = Rf_asCharacterFactor(val);
	val = ans;
    }

    if (Rf_isList(val) || Rf_isNull(val)) {
	n = Rf_length(val);
    } else {
	n = LENGTH(val);
    }

    val_type = TYPEOF(val);


    bgn = BodyHasBraces(body);

    /* bump up links count of sequence to avoid modification by loop code */
    INCREMENT_LINKS(val);

    Environment* env = SEXP_downcast<Environment*>(rho);
    Environment::LoopScope loopscope(env);
    for (i = 0; i < n; i++) {
	Evaluator::maybeCheckForUserInterrupts();
	DO_LOOP_RDEBUG(call, op, args, rho, bgn);

	switch (val_type) {

	case EXPRSXP:
	    /* make sure loop variable is not modified via other vars */
		ENSURE_NAMEDMAX(XVECTOR_ELT(val, i));
	    /* defineVar is used here and below rather than setVar in
	       case the loop code removes the variable. */
	    Rf_defineVar(sym, XVECTOR_ELT(val, i), rho);
	    break;
	case VECSXP:
	    /* make sure loop variable is not modified via other vars */
		ENSURE_NAMEDMAX(VECTOR_ELT(val, i));
	    /* defineVar is used here and below rather than setVar in
	       case the loop code removes the variable. */
	    Rf_defineVar(sym, VECTOR_ELT(val, i), rho);
	    break;

	case LISTSXP:
	    /* make sure loop variable is not modified via other vars */
	    ENSURE_NAMEDMAX(CAR(val));
	    Rf_defineVar(sym, CAR(val), rho);
	    val = CDR(val);
	    break;

	default:
                switch (val_type) {
                case LGLSXP:
                    v = ALLOC_LOOP_VAR(v, val_type);
                    SET_SCALAR_LVAL(v, Rboolean(LOGICAL_ELT(val, i)));
                    break;
                case INTSXP:
                    v = ALLOC_LOOP_VAR(v, val_type);
                    SET_SCALAR_IVAL(v, INTEGER_ELT(val, i));
                    break;
                case REALSXP:
                    v = ALLOC_LOOP_VAR(v, val_type);
                    SET_SCALAR_DVAL(v, REAL_ELT(val, i));
                    break;
                case CPLXSXP:
                    v = ALLOC_LOOP_VAR(v, val_type);
                    SET_SCALAR_CVAL(v, COMPLEX_ELT(val, i));
                    break;
                case STRSXP:
                    v = ALLOC_LOOP_VAR(v, val_type);
                    SET_STRING_ELT(v, 0, STRING_ELT(val, i));
                    break;
                case RAWSXP:
                    v = ALLOC_LOOP_VAR(v, val_type);
                    SET_SCALAR_BVAL(v, RAW(val)[i]);
                    break;
                default:
                    Rf_errorcall(call, _("invalid for() loop sequence"));
                }
            Rf_defineVar(sym, v, rho);
	}
	try {
	    BailoutContext bcntxt;
	    ans = Rf_eval(body, rho);
	}
	catch (LoopException& lx) {
	    if (lx.environment() != env)
		throw;
	    if (lx.next())
		continue;
	    else break;
	}
	if (ans && ans->sexptype() == BAILSXP) {
	    LoopBailout* lbo = dynamic_cast<LoopBailout*>(ans.get());
	    if (lbo) {
		if (lbo->environment() != rho)
		    abort();
		if (lbo->next())
		    continue;
		else break;
	    } else {  // This must be a ReturnBailout:
		SET_ENV_RDEBUG(rho, dbg);
		return propagateBailout(ans.get());
	    }
	}
    }
    SET_ENV_RDEBUG(rho, dbg);
    return nullptr;
}

HIDDEN SEXP do_for(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    return GCStackFrameBoundary::withStackFrameBoundary(
	[=]() { return do_for_impl(call, op, args, rho); });
}

static SEXP do_while_impl(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    Rboolean dbg;
    volatile int bgn;
    GCStackRoot<> t;
    volatile SEXP body;

    dbg = ENV_RDEBUG(rho);
    /* rho FIXME
    if (R_jit_enabled > 2 && !dbg && ! R_PendingPromises) {
	R_compileAndExecute(call, rho);
	return nullptr;
    }
    */

    body = CADR(args);
    bgn = BodyHasBraces(body);

    Environment* env = SEXP_downcast<Environment*>(rho);
    Environment::LoopScope loopscope(env);

    while (asLogicalNoNA(Rf_eval(CAR(args), rho), call, rho)) {
	Evaluator::maybeCheckForUserInterrupts();
	RObject* ans;
	DO_LOOP_RDEBUG(call, op, args, rho, bgn);
	try {
	    BailoutContext bcntxt;
	    ans = Rf_eval(body, rho);
	}
	catch (LoopException& lx) {
	    if (lx.environment() != env)
		throw;
	    if (lx.next())
		continue;
	    else break;
	}
	if (ans && ans->sexptype() == BAILSXP) {
	    LoopBailout* lbo = dynamic_cast<LoopBailout*>(ans);
	    if (lbo) {
		if (lbo->environment() != rho)
		    abort();
		if (lbo->next())
		    continue;
		else break;
	    } else {  // This must be a ReturnBailout:
		SET_ENV_RDEBUG(rho, dbg);
		return propagateBailout(ans);
	    }
	}
    }
    SET_ENV_RDEBUG(rho, dbg);
    return nullptr;
}

HIDDEN SEXP do_while(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    return GCStackFrameBoundary::withStackFrameBoundary(
	[=]() {  return do_while_impl(call, op, args, rho); });
}

static SEXP do_repeat_impl(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    Rboolean dbg;
    volatile int bgn;
    GCStackRoot<> t;
    volatile SEXP body;

    dbg = ENV_RDEBUG(rho);
    /* rho FIXME
    if (R_jit_enabled > 2 && !dbg && ! R_PendingPromises) {
	R_compileAndExecute(call, rho);
	return nullptr;
    }
    */

    body = CAR(args);
    bgn = BodyHasBraces(body);

    Environment* env = SEXP_downcast<Environment*>(rho);
    Environment::LoopScope loopscope(env);
    for (;;) {
	Evaluator::maybeCheckForUserInterrupts();
	RObject* ans;
	DO_LOOP_RDEBUG(call, op, args, rho, bgn);
	try {
	    BailoutContext bcntxt;
	    ans = Rf_eval(body, rho);
	}
	catch (LoopException& lx) {
	    if (lx.environment() != env)
		throw;
	    if (lx.next())
		continue;
	    else break;
	}
	if (ans && ans->sexptype() == BAILSXP) {
	    LoopBailout* lbo = dynamic_cast<LoopBailout*>(ans);
	    if (lbo) {
		if (lbo->environment() != rho)
		    abort();
		if (lbo->next())
		    continue;
		else break;
	    } else {  // This must be a ReturnBailout:
		SET_ENV_RDEBUG(rho, dbg);
		return propagateBailout(ans);
	    }
	}
    }

    SET_ENV_RDEBUG(rho, dbg);
    return nullptr;
}

HIDDEN SEXP do_repeat(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    return GCStackFrameBoundary::withStackFrameBoundary(
	[=]() { return do_repeat_impl(call, op, args, rho); });
}

HIDDEN SEXP do_break(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    Environment* env = SEXP_downcast<Environment*>(rho);
    if (!env->loopActive())
	Rf_error(_("no loop to break from"));
    LoopBailout* lbo = new LoopBailout(env, PRIMVAL(op) == 1);
    return propagateBailout(lbo);
}

RObject* do_paren(Expression* call,
				   const BuiltInFunction* op,
				   RObject* x_)
{
    return x_;
}

HIDDEN SEXP do_begin(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP s = nullptr;
    if (args != nullptr) {
	GCStackRoot<> srcrefs(getBlockSrcrefs(call));
	PROTECT(srcrefs);
	int i = 1;
	while (args != nullptr) {
	    R_Srcref = getSrcref(srcrefs, i++);
	    if (ENV_RDEBUG(rho)) {
		Rf_SrcrefPrompt("debug", R_Srcref);
		Rf_PrintValue(CAR(args));
		do_browser(nullptr, nullptr, nullptr, rho);
	    }
	    {
		BailoutContext bcntxt;
		s = Rf_eval(CAR(args), rho);
	    }
	    if (s && s->sexptype() == BAILSXP) {
		R_Srcref = nullptr;
		return propagateBailout(s);
	    }
	    args = CDR(args);
	}
	R_Srcref = nullptr;
	UNPROTECT(1); /* srcrefs */
    }
    return s;
}


HIDDEN SEXP do_return(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    GCStackRoot<> v;

    if (args == nullptr) /* zero arguments provided */
	v = nullptr;
    else if (CDR(args) == nullptr) /* one argument */
	v = Rf_eval(CAR(args), rho);
    else {
	v = nullptr; /* to avoid compiler warnings */
	Rf_errorcall(call, _("multi-argument returns are not permitted"));
    }

    Environment* envir = SEXP_downcast<Environment*>(rho);
    if (!envir->canReturn())
	Rf_error(_("no function to return from, jumping to top level"));
    ReturnBailout* rbo = new ReturnBailout(envir, v);
    return propagateBailout(rbo);
}

/* Declared with a variable number of args in names.cpp */
HIDDEN SEXP do_function(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    GCStackRoot<> rval;

    if (TYPEOF(op) == PROMSXP) {
	op = forcePromise(op);
	ENSURE_NAMEDMAX(op);
    }
    if (Rf_length(args) < 2) WrongArgCount("function");
    SEXP formals = CAR(args);
    if (formals && formals->sexptype() != LISTSXP)
	Rf_error(_("invalid formal argument list for 'function'"));
    rval = Rf_mkCLOSXP(formals, CADR(args), rho);
    SEXP srcref = CADDR(args);
    if (!Rf_isNull(srcref)) Rf_setAttrib(rval, Symbols::SrcrefSymbol, srcref);
    return rval;
}


/*
 *  Assignments for complex LVAL specifications. This is the stuff that
 *  nightmares are made of ...	Note that "evalseq" preprocesses the LHS
 *  of an assignment.  Given an expression, it builds a list of partial
 *  values for the exression.  For example, the assignment x$a[3] <- 10
 *  with LHS x$a[3] yields the (improper) list:
 *
 *	 (Rf_eval(x$a[3])	Rf_eval(x$a)  Rf_eval(x)  .  x)
 *
 *  (Note the terminating symbol).  The partial evaluations are carried
 *  out efficiently using previously computed components.
 */

// rho here (necessarily) uses a proper list, with x as the CAR of
// the last element.

/*
  For complex superassignment  x[y==z]<<-w
  we want x required to be nonlocal, y,z, and w permitted to be local or
  nonlocal.
*/

static PairList* evalseq(SEXP expr, SEXP rho, int forcelocal,
			 Frame::Binding* tmploc)
{
    GCStackRoot<> exprrt(expr);
    if (Rf_isNull(expr))
	Rf_error(_("invalid (NULL) left side of assignment"));
    if (Rf_isSymbol(expr)) {
	GCStackRoot<> nval;
	if(forcelocal) {
	    nval = EnsureLocal(expr, rho);
	}
	else {/* now we are down to the target symbol */
	  nval = Rf_eval(expr, ENCLOS(rho));
	}
	return PairList::cons(nval, PairList::cons(expr));
    }
    else if (Rf_isLanguage(expr)) {
	Expression* exprn = SEXP_downcast<Expression*>(expr);
	GCStackRoot<PairList> val(evalseq(exprn->tail()->car(), rho, forcelocal, tmploc));
	tmploc->assign(val->car());
	GCStackRoot<PairList> nexprargs(
	    PairList::cons(const_cast<Symbol*>(tmploc->symbol()),
			   exprn->tail()->tail()));
	GCStackRoot<Expression> nexpr(new Expression(exprn->car(), nexprargs));
	GCStackRoot<> nval(Rf_eval(nexpr, rho));
	return PairList::cons(nval, val);
    }
    else Rf_error(_("target of assignment expands to non-language object"));
    return nullptr;	/*NOTREACHED*/
}

/* Main entry point for complex assignments */
/* We have checked to see that CAR(args) is a LANGSXP */

static const char * const asym[] = {":=", "<-", "<<-", "="};

/* This function stores the current assignment target in the saved
   binding location. It duplicates if necessary to make sure
   replacement functions are always called with a target with NAMED ==
   1. The SET_CAR is intended to protect against possible GC in
   R_SetVarLocValue; this might occur it the binding is an active
   binding. */
static void SET_TEMPVARLOC_FROM_CAR(Frame::Binding* loc, PairList* lhs) {
    SEXP v = lhs->car();
    if (NAMED(v) == 2) {
	v = Rf_duplicate(v);
	ENSURE_NAMED(v);
	lhs->setCar(v);
    }
    loc->assign(v);
}

/* This macro makes sure the RHS NAMED value is 0 or NAMEDMAX. This is
   necessary to make sure the RHS value returned by the assignment
   expression is correct when the RHS value is part of the LHS
   object. */
#define FIXUP_RHS_NAMED(r) do { \
	SEXP __rhs__ = (r); \
	if (NAMED(__rhs__)) \
	    ENSURE_NAMEDMAX(__rhs__); \
    } while (0)

// Functions and data auxiliary to applydefine():
namespace {
    std::map<const Symbol*, Symbol*> sym2replac;

    // Given a Symbol "foo", this function returns the Symbol "foo<-":
    Symbol* func2ReplacementFunc(const Symbol* fsym)
    {
	Symbol* ans = sym2replac[fsym];
	if (!ans) {
	    std::string replacname = fsym->name()->stdstring() + "<-";
	    ans = sym2replac[fsym] = Symbol::obtain(replacname);
	}
	return ans;
    }

    std::vector<Symbol*> obtainAssignSyms()
    {
	std::vector<Symbol*> ans(4);
	for (size_t i = 0; i < 4; ++i)
	    ans[i] = Symbol::obtain(asym[i]);
	return ans;
    }
}

// applydefine() handles the case where the left-hand side of an
// assignment is not a symbol: specifically the cases where it is an
// Expression (LANGSXP), or null (which gives an error).
//
// Section 13.5 of the 'yellow book' (Chambers' Software for Data
// Analysis) gives useful background.

SEXP applydefine(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    GCStackRoot<> expr(CAR(args));
    SEXP functor = CAR(expr);

    /*  It's important that the rhs get evaluated first because
	assignment is right associative i.e.  a <- b <- c is parsed as
	a <- (b <- c).  */

    GCStackRoot<> rhs, saverhs;
    saverhs = rhs = Rf_eval(CADR(args), rho);

    if (PRIMVAL(op) == 2)
	Environment::monitorLeaks(rhs);

    /*  FIXME: We need to ensure that this works for hashed
	environments.  This code only works for unhashed ones.  the
	syntax error here is a deliberate marker so I don't forget that
	this needs to be done.  The code used in "missing" will help
	here.  */

    /*  FIXME: This strategy will not work when we are working in the
	data frame defined by the system hash table.  The structure there
	is different.  Should we special case here?  */

    /*  We need a temporary variable to hold the intermediate values
	in the computation.  For efficiency reasons we record the
	location where this variable is stored.  We need to protect
	the location in case the biding is removed from its
	environment by user code or an assignment within the
	assignment arguments */

    /*  There are two issues with the approach here:

	    A complex assignment within a complex assignment, like
	    f(x, y[] <- 1) <- 3, can cause the value temporary
	    variable for the outer assignment to be overwritten and
	    then removed by the inner one.  This could be addressed by
	    using multiple temporaries or using a promise for this
	    variable as is done for the RHS.  Printing of the
	    replacement function call in error messages might then need
	    to be adjusted.

	    With assignments of the form f(g(x, z), y) <- w the value
	    of 'z' will be computed twice, once for a call to g(x, z)
	    and once for the call to the replacement function g<-.  It
	    might be possible to address this by using promises.
	    Using more temporaries would not work as it would mess up
	    replacement functions that use substitute and/or
	    nonstandard evaluation (and there are packages that do
	    that -- igraph is one).

	    LT */

    FIXUP_RHS_NAMED(rhs);

    if (rho == R_BaseNamespace)
	Rf_errorcall(call, _("cannot do complex assignments in base namespace"));
    if (rho == R_BaseEnv)
	Rf_errorcall(call, _("cannot do complex assignments in base environment"));
    Rf_defineVar(Symbols::TmpvalSymbol, nullptr, rho);
    Frame::Binding* tmploc
	= SEXP_downcast<Environment*>(rho)->frame()->binding(TmpvalSymbol);
    /* Now use try-catch to remove it when we are done, even in the
     * case of an error.  This all helps Rf_error() provide a better call.
     */
    try {
	/*  Do a partial evaluation down through the LHS. */
	GCStackRoot<PairList> lhs(
	    evalseq(CADR(expr), rho,
		    PRIMVAL(op)==1 || PRIMVAL(op)==3, tmploc));

	GCStackRoot<> rhsprom(Promise::createEvaluatedPromise(CADR(args), rhs));

	SEXP firstarg = CADR(expr);
	while (Rf_isLanguage(firstarg)) {
	    GCStackRoot<> tmp;
	    if (TYPEOF(functor) == SYMSXP)
		tmp = func2ReplacementFunc(SEXP_downcast<Symbol*>(functor));
	    else {
		/* check for and handle assignments of the form
		   foo::bar(x) <- y or foo:::bar(x) <- y */
		if (TYPEOF(functor) == LANGSXP) {
		    SEXP funchead = CAR(functor);
		    if ((funchead == Symbols::DoubleColonSymbol
			 || funchead == Symbols::TripleColonSymbol)
			&& Rf_length(functor) == 3) {
			SEXP arg2 = CADDR(functor);
			if (TYPEOF(arg2) == SYMSXP) {
			    const Symbol* fsym = SEXP_downcast<Symbol*>(arg2);
			    tmp = func2ReplacementFunc(fsym);
			    tmp = Rf_lang3(funchead, CADR(functor), tmp);
			}
		    }
		}
		if (!tmp)
		    Rf_error(_("invalid function in complex assignment"));
	    }
	    SET_TEMPVARLOC_FROM_CAR(tmploc, lhs);
	    rhs = replaceCall(tmp, Symbols::TmpvalSymbol, CDDR(expr), rhsprom);
	    rhs = Rf_eval(rhs, rho);
	    SET_PRVALUE(rhsprom, rhs);
	    // Try doing without this in rho:
	    // SET_PRCODE(rhsprom, rhs); /* not good but is what we have been doing */
	    lhs = lhs->tail();
	    expr = firstarg;
	    functor = CAR(expr);
	    firstarg = CADR(expr);
	}
	GCStackRoot<> afun;
	if (TYPEOF(functor) == SYMSXP)
	    afun = func2ReplacementFunc(SEXP_downcast<Symbol*>(functor));
	else {
	    /* check for and handle assignments of the form
	       foo::bar(x) <- y or foo:::bar(x) <- y */
	    if (TYPEOF(functor) == LANGSXP) {
		SEXP funchead = CAR(functor);
		if ((funchead == Symbols::DoubleColonSymbol
		     || funchead == Symbols::TripleColonSymbol)
		    && Rf_length(functor) == 3) {
		    SEXP arg2 = CADDR(functor);
		    if (TYPEOF(arg2) == SYMSXP) {
			const Symbol* fsym = SEXP_downcast<Symbol*>(arg2);
			afun = func2ReplacementFunc(fsym);
			afun = Rf_lang3(funchead, CADR(functor), afun);
		    }
		}
	    }
	    if (!afun)
		Rf_error(_("invalid function in complex assignment"));
	}
	SET_TEMPVARLOC_FROM_CAR(tmploc, lhs);
	static std::vector<Symbol*> assignsyms = obtainAssignSyms();
	// Second arg in the foll. changed in rho at r253 (2008-03-18):
	expr = assignCall(assignsyms[PRIMVAL(op)], CADR(lhs),
			  afun, Symbols::TmpvalSymbol, CDDR(expr), rhsprom);
	expr = Rf_eval(expr, rho);
    }
    catch (...) {
	Rf_unbindVar(Symbols::TmpvalSymbol, rho);
	throw;
    }

    Rf_unbindVar(Symbols::TmpvalSymbol, rho);
#ifdef CONSERVATIVE_COPYING /* not default */
    return Rf_duplicate(saverhs);
#else
    /* we do not duplicate the value, so to be conservative mark the
       value as NAMED = NAMEDMAX */
    ENSURE_NAMEDMAX(saverhs);
    return saverhs;
#endif
}

/*  Assignment in its various forms  */

HIDDEN SEXP do_set(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP lhs, rhs;

    if (args == nullptr ||
	CDR(args) == nullptr ||
	CDDR(args) != nullptr)
	WrongArgCount(asym[PRIMVAL(op)]);

    lhs = CAR(args);

    switch (TYPEOF(lhs)) {
    case STRSXP:
	lhs = Rf_installTrChar(STRING_ELT(lhs, 0));
	/* fall through */
    case SYMSXP:
	rhs = Rf_eval(CADR(args), rho);
	INCREMENT_NAMED(rhs);
	if (PRIMVAL(op) == 2)                       /* <<- */
	    Rf_setVar(lhs, rhs, ENCLOS(rho));
	else                                        /* <-, = */
	    Rf_defineVar(lhs, rhs, rho);
	R_Visible = FALSE;
	return rhs;
    case LANGSXP:
	R_Visible = FALSE;
	return applydefine(call, op, args, rho);
    default:
	Rf_errorcall(call, _("invalid (do_set) left-hand side to assignment"));
    }

    return nullptr;/*NOTREACHED*/
}


static SEXP VectorToPairListNamed(SEXP x)
{
    SEXP xptr, xnew, xnames;
    int i, len = 0, named;
    const void *vmax = vmaxget();

    PROTECT(x);
    PROTECT(xnames = Rf_getAttrib(x, Symbols::NamesSymbol)); /* isn't this protected via x? */
    named = (xnames != nullptr);
    if(named)
	for (i = 0; i < Rf_length(x); i++)
	    if (R_CHAR(STRING_ELT(xnames, i))[0] != '\0') len++;

    if(len) {
	PROTECT(xnew = Rf_allocList(len));
	xptr = xnew;
	for (i = 0; i < Rf_length(x); i++) {
	    if (R_CHAR(STRING_ELT(xnames, i))[0] != '\0') {
		SETCAR(xptr, VECTOR_ELT(x, i));
		SET_TAG(xptr, Rf_installTrChar(STRING_ELT(xnames, i)));
		xptr = CDR(xptr);
	    }
	}
	UNPROTECT(1);
    } else xnew = Rf_allocList(0);
    UNPROTECT(2);
    vmaxset(vmax);
    return xnew;
}

#define simple_as_environment(arg) (IS_S4_OBJECT(arg) && (TYPEOF(arg) == S4SXP) ? R_getS4DataSlot(arg, ENVSXP) : nullptr)

/* "eval": Evaluate the first argument
   in the environment specified by the second argument. */

HIDDEN SEXP do_eval(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP encl, x;
    volatile SEXP expr, env;

    int frame;

    expr = CAR(args);
    env = CADR(args);
    encl = CADDR(args);
    SEXPTYPE tEncl = TYPEOF(encl);
    if (Rf_isNull(encl)) {
	/* This is supposed to be defunct, but has been kept here
	   (and documented as such) */
	encl = R_BaseEnv;
    } else if ( !Rf_isEnvironment(encl) &&
		!Rf_isEnvironment((encl = simple_as_environment(encl))) ) {
	Rf_error(_("invalid '%s' argument of type '%s'"),
	      "enclos", Rf_type2char(tEncl));
    }
    if(IS_S4_OBJECT(env) && (TYPEOF(env) == S4SXP))
	env = R_getS4DataSlot(env, ANYSXP); /* usually an ENVSXP */
    switch(TYPEOF(env)) {
    case NILSXP:
	env = encl;     /* so eval(expr, NULL, encl) works */
	/* falls through */
    case ENVSXP:
	PROTECT(env);	/* so we can unprotect 2 at the end */
	break;
    case LISTSXP:
	/* This usage requires all the pairlist to be named */
	env = Rf_NewEnvironment(nullptr, Rf_duplicate(env), encl);
	PROTECT(env);
	break;
    case VECSXP:
	/* PR#14035 */
	x = VectorToPairListNamed(env);
	for (SEXP xptr = x ; xptr != nullptr ; xptr = CDR(xptr))
	    ENSURE_NAMEDMAX(CAR(xptr));
	env = Rf_NewEnvironment(nullptr, x, encl);
	PROTECT(env);
	break;
    case INTSXP:
    case REALSXP:
	if (Rf_length(env) != 1)
	    Rf_error(_("numeric 'envir' arg not of length one"));
	frame = Rf_asInteger(env);
	if (frame == R_NaInt)
	    Rf_error(_("invalid '%s' argument of type '%s'"),
		  "envir", Rf_type2char(TYPEOF(env)));
	PROTECT(env = R_sysframe(frame, R_GlobalContext()));
	break;
    default:
	Rf_error(_("invalid '%s' argument of type '%s'"),
	      "envir", Rf_type2char(TYPEOF(env)));
    }

    /* Rf_isLanguage include NILSXP, and that does not need to be
       evaluated
    if (Rf_isLanguage(expr) || Rf_isSymbol(expr) || isByteCode(expr)) { */
    if (TYPEOF(expr) == LANGSXP || TYPEOF(expr) == SYMSXP) {
	PROTECT(expr);
	{
	    Expression* callx = SEXP_downcast<Expression*>(call);
	    Environment* call_env = SEXP_downcast<Environment*>(rho);
	    FunctionBase* func = SEXP_downcast<FunctionBase*>(op);
	    Environment* working_env = SEXP_downcast<Environment*>(env);
	    ClosureContext cntxt(callx, call_env, func, working_env);
	    Environment::ReturnScope returnscope(working_env);
	    try {
		expr = Rf_eval(expr, env);
	    }
	    catch (ReturnException& rx) {
		if (rx.environment() != working_env)
		    throw;
		expr = rx.value();
	    }
	}
	UNPROTECT(1);
    }
    else if (TYPEOF(expr) == EXPRSXP) {
	int i, n;
	SEXP srcrefs = getBlockSrcrefs(expr);
	PROTECT(expr);
	n = LENGTH(expr);
	SEXP tmp = nullptr;
	{
	    Expression* callx = SEXP_downcast<Expression*>(call);
	    Environment* call_env = SEXP_downcast<Environment*>(rho);
	    FunctionBase* func = SEXP_downcast<FunctionBase*>(op);
	    Environment* working_env = SEXP_downcast<Environment*>(env);
	    ClosureContext cntxt(callx, call_env, func, working_env);
	    Environment::ReturnScope returnscope(working_env);
	    try {
		for (i = 0 ; i < n ; i++) {
		    R_Srcref = getSrcref(srcrefs, i);
		    tmp = Rf_eval(XVECTOR_ELT(expr, i), env);
		}
	    }
	    catch (ReturnException& rx) {
		if (rx.environment() != working_env)
		    throw;
		tmp = rx.value();
	    }
	}
	UNPROTECT(1);
	expr = tmp;
    }
    else if( TYPEOF(expr) == PROMSXP ) {
	expr = Rf_eval(expr, rho);
    } /* else expr is returned unchanged */
    UNPROTECT(1);
    return expr;
}

/* This is a special .Internal */
HIDDEN SEXP do_withVisible(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, nm, ret;

    x = CAR(args);
    x = Rf_eval(x, rho);
    PROTECT(x);
    PROTECT(ret = ListVector::create(2));
    PROTECT(nm = StringVector::create(2));
    SET_STRING_ELT(nm, 0, Rf_mkChar("value"));
    SET_STRING_ELT(nm, 1, Rf_mkChar("visible"));
    SET_VECTOR_ELT(ret, 0, x);
    SET_VECTOR_ELT(ret, 1, Rf_ScalarLogical(R_Visible));
    Rf_setAttrib(ret, Symbols::NamesSymbol, nm);
    UNPROTECT(3);
    return ret;
}

/* This is a special .Internal */
HIDDEN SEXP do_recall(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    ClosureContext *cptr;
    SEXP s, ans ;
    cptr = R_GlobalContext();
    /* get the args supplied */
    while (cptr && cptr->workingEnvironment() != rho) {
	cptr = ClosureContext::innermost(cptr->nextOut());
    }
    ClosureContext* args_cptr = cptr;

    /* get the env recall was called from */
    s = ClosureContext::innermost()->callEnvironment();
    while (cptr && cptr->workingEnvironment() != s) {
	cptr = ClosureContext::innermost(cptr->nextOut());
    }
    if (cptr == nullptr)
	Rf_error(_("'Recall' called from outside a closure"));
    const ArgList& arglist = args_cptr->promiseArgs();

    /* If the function has been recorded in the context, use it
       otherwise search for it by name or evaluate the expression
       originally used to get it.
    */
    if (cptr->function() != nullptr)
	PROTECT(s = const_cast<FunctionBase*>(cptr->function()));
    else if( TYPEOF(CAR(const_cast<Expression*>(cptr->call()))) == SYMSXP)
	PROTECT(s = Rf_findFun(CAR(const_cast<Expression*>(cptr->call())), cptr->callEnvironment()));
    else
	PROTECT(s = Rf_eval(CAR(const_cast<Expression*>(cptr->call())), cptr->callEnvironment()));
    if (TYPEOF(s) != CLOSXP)
	Rf_error(_("'Recall' called from outside a closure"));
    Closure* closure = SEXP_downcast<Closure*>(s);
    ans = cptr->call()->evaluateFunctionCall(closure, cptr->callEnvironment(),
                                             arglist);
    UNPROTECT(1);
    return ans;
}

static bool isDefaultMethod(const Expression* call) {
    RObject* callcar = call->car();
    if (callcar->sexptype() != SYMSXP) {
	return false;
    }
    const String* symbol_name = SEXP_downcast<Symbol*>(callcar)->name();

    static const size_t suffix_length = strlen(".default");
    if (symbol_name->size() < suffix_length)
	return false;
    return strcmp(symbol_name->c_str() + symbol_name->size() - suffix_length,
		  ".default") == 0;
}
#if 0
/* A version of DispatchOrEval that checks for possible S4 methods for
 * any argument, not just the first.  Used in the code for `c()` in do_c()
 * and previously used in the code for `[` in do_subset.
 * Differs in that all arguments are evaluated
 * immediately, rather than after the call to R_possible_dispatch.
 */
HIDDEN
int DispatchAnyOrEval(SEXP call, SEXP op, const char *generic, SEXP args,
		      SEXP rho, SEXP *ans, int dropmissing, int argsevald)
{
    if(R_has_methods(op)) {
	SEXP argValue, el,  value;
	/* Rboolean hasS4 = FALSE; */
	int nprotect = 0, dispatch;
	if(!argsevald) {
	    PROTECT(argValue = evalArgs(args, rho, dropmissing, call, 0));
	    nprotect++;
	    argsevald = TRUE;
	}
	else argValue = args;
	for(el = argValue; el != nullptr; el = CDR(el)) {
	    if(IS_S4_OBJECT(CAR(el))) {
		value = R_possible_dispatch(call, op, argValue, rho, TRUE);
		if(value) {
		    *ans = value;
		    UNPROTECT(nprotect);
		    return 1;
		}
		else break;
	    }
	}
	 /* else, use the regular DispatchOrEval, but now with evaluated args */
	dispatch = Rf_DispatchOrEval(call, op, generic, argValue, rho, ans, dropmissing, argsevald);
	UNPROTECT(nprotect);
	return dispatch;
    }
    return Rf_DispatchOrEval(call, op, generic, args, rho, ans, dropmissing, argsevald);
}
#endif

/* Rf_DispatchOrEval is used in internal functions which dispatch to
 * object methods (e.g. "[" or "[[").  The code either builds promises
 * and dispatches to the appropriate method, or it evaluates the
 * (unevaluated) arguments it comes in with and returns to the caller.

 * To call this an ugly hack would be to insult all existing ugly hacks
 * at large in the world.
 *
 * Functions that use this are:
 *   [, [[, $, [<-, [[<-, $<-, @<-, rep
 */
HIDDEN
std::pair<bool, SEXP>
Rf_DispatchOrEval(const Expression* call, const BuiltInFunction* func,
                  ArgList* arglist, Environment* callenv,
                  MissingArgHandling dropmissing)
{
    // Rf_DispatchOrEval is called very frequently, most often in cases
    // where no dispatching is needed and the Rf_isObject or the
    // string-based pre-test fail.  To avoid degrading performance it
    // is therefore necessary to avoid creating promises in these
    // cases.  The pre-test does require that we look at the first
    // argument, so that needs to be evaluated.  The complicating
    // factor is that the first argument might come in with a "..."
    // and that there might be other arguments in the "..." as well.
    // LT
    assert(arglist->status() == ArgList::RAW);
    GCStackRoot<> x(arglist->firstArg(callenv).second);

    // try to dispatch on the object
    if (x && x->hasClass()) {
	// Try for formal method.
	if (x->isS4Object() && R_has_methods(func)) {
	    // create a promise to pass down to applyClosure
	    arglist->wrapInPromises(callenv, call);

	    /* This means S4 dispatch */
            auto pr = R_possible_dispatch(call, func, *arglist, callenv);
	    if (pr.first) {
                return pr;
	    }
	}
	if (!isDefaultMethod(call)) {
	    if (arglist->status() != ArgList::PROMISED)
		arglist->wrapInPromises(callenv, call);
	    /* The context set up here is needed because of the way
	       Rf_usemethod() is written.  Rf_DispatchGroup() repeats some
	       internal Rf_usemethod() code and avoids the need for a
	       context; perhaps the Rf_usemethod() code should be
	       refactored so the contexts around the Rf_usemethod() calls
	       in this file can be removed.

	       Using callenv for current and calling environment can be
	       confusing for things like sys.parent() calls captured
	       in promises (Gabor G had an example of this).  Also,
	       since the context is established without a SETJMP using
	       an R-accessible environment allows a segfault to be
	       triggered (by something very obscure, but still).
	       Hence here and in the other Rf_usemethod() uses below a
	       new environment working_env is created and used.  LT */
	    GCStackRoot<Frame> frame(Frame::closureWorkingFrame(*arglist));
	    Environment* working_env = new Environment(callenv, frame);
	    ClosureContext cntxt(call, callenv, func, working_env);
	    const char* generic = func->name();
            auto dispatched = Rf_usemethod(generic, x, call,
                                           working_env, callenv,
                                           Environment::base());
	    if (dispatched.first)
                return dispatched;
	}
    }
    arglist->evaluate(callenv, dropmissing);
    return std::make_pair(false, nullptr);
}

HIDDEN
std::pair<bool, SEXP>
Rf_Dispatch(const Expression* call, const BuiltInFunction* func,
            const ArgList& arglist, Environment* callenv)
{
    assert(arglist.status() == ArgList::EVALUATED);
    GCStackRoot<> x(arglist.get(0));

    // try to dispatch on the object
    if (x && x->hasClass()) {
	// Try for formal method.
	if (x->isS4Object() && R_has_methods(func)) {
	    // create a promise to pass down to applyClosure
            ArgList promises(arglist);
            promises.wrapInPromises(callenv, call);

	    /* This means S4 dispatch */
            auto pr = R_possible_dispatch(call, func, promises, callenv);
	    if (pr.first) {
                return pr;
	    }
	}
	if (!isDefaultMethod(call)) {
            ArgList promises(arglist);
            promises.wrapInPromises(callenv, call);
	    GCStackRoot<Frame> frame(Frame::closureWorkingFrame(promises));
	    Environment* working_env = new Environment(callenv, frame);
	    ClosureContext cntxt(call, callenv, func, working_env);
	    const char* generic = func->name();
            auto dispatched = Rf_usemethod(generic, x, call,
                                           working_env, callenv,
                                           Environment::base());
	    if (dispatched.first)
                return dispatched;
	}
    }
    return std::make_pair(false, nullptr);
}

HIDDEN
int Rf_DispatchGroup(const char* group, SEXP call, SEXP op, SEXP args, SEXP rho,
		     SEXP *ans) {
  auto result = Rf_DispatchGroup(group,
                                 SEXP_downcast<Expression*>(call),
                                 SEXP_downcast<BuiltInFunction*>(op),
                                 ArgList(SEXP_downcast<PairList*>(args),
                                         ArgList::EVALUATED),
                                 SEXP_downcast<Environment*>(rho));
  if (result.first)
      *ans = result.second;
  return result.first;
}


HIDDEN
std::pair<bool, SEXP>
Rf_DispatchGroup(const char *group, const Expression* call,
                 const BuiltInFunction* op,
                 ArgList&& args,
                 Environment* callenv)
{
    assert(group != nullptr);

    std::size_t numargs = args.size();
    if (numargs == 0)
      return std::make_pair(false, nullptr);
    RObject* arg1val = args.get(0);
    RObject* arg2val = (numargs > 1 ? args.get(1) : nullptr);

    /* pre-test to avoid string computations when there is nothing to
       dispatch on because either there is only one argument and it
       isn't an object or there are two or more arguments but neither
       of the first two is an object -- both of these cases would be
       rejected by the code following the string examination code
       below */
    if (!((arg1val && arg1val->hasClass())
	  || (arg2val && arg2val->hasClass())))
        return std::make_pair(false, nullptr);

    bool isOps = (streql(group, "Ops"));

    /* try for formal method */
    bool useS4 = ((arg1val && arg1val->isS4Object())
		  || (arg2val && arg2val->isS4Object()));
    if (useS4 && R_has_methods(op)) {
        /* Remove argument names to ensure positional matching */
        if (isOps)
            args.stripTags();
        auto dispatched = R_possible_dispatch(call, op, args, callenv);
        if (dispatched.first) {
          return dispatched;
	}
	/* else go on to look for S3 methods */
    }

    /* check whether we are processing the default method */
    if (isDefaultMethod(call)) {
      return std::make_pair(false, nullptr);
    }

    std::size_t nargs = (isOps ? numargs : 1);
    string generic(op->name());

    GCStackRoot<S3Launcher>
	l(S3Launcher::create(arg1val, generic, group,
			     callenv, Environment::base(), false));
    if (l && arg1val->isS4Object() && l->locInClasses() > 0
	&& Rf_isBasicClass(Rf_translateChar(l->className()))) {
	/* This and the similar test below implement the strategy
	 for S3 methods selected for S4 objects.  See ?Methods */
        RObject* value = arg1val;
	if (NAMED(value))
	    ENSURE_NAMEDMAX(value);
	value = R_getS4DataSlot(value, S4SXP); /* the .S3Class obj. or NULL*/
	if (value) { /* use the S3Part as the inherited object */
            args.set(0, value);
	    arg1val = value;
	}
    }

    GCStackRoot<S3Launcher> r;
    if (nargs == 2)
	r = S3Launcher::create(arg2val, generic, group,
			       callenv, Environment::base(), false);
    if (r && arg2val->isS4Object() && r->locInClasses() > 0
	&& Rf_isBasicClass(Rf_translateChar(r->className()))) {
        RObject* value = arg2val;
	if(NAMED(value))
	    ENSURE_NAMEDMAX(value);
	value = R_getS4DataSlot(value, S4SXP);
	if (value) {
            args.set(1, value);
	    arg2val = value;
	}
    }

    if (!l && !r) {
        /* no generic or group method so use default*/
        return std::make_pair(0, nullptr);
    }

    if (l && r && l->function() != r->function()) {
	/* special-case some methods involving difftime */
	const String &lname = *l->symbol()->name();
	const String &rname = *r->symbol()->name();
	if (rname == "Ops.difftime"
	    && (lname == "+.POSIXt" || lname == "-.POSIXt"
		|| lname == "+.Date" || lname == "-.Date"))
	    r = nullptr;
	else if (lname == "Ops.difftime"
		 && (rname == "+.POSIXt" || rname == "+.Date"))
	    l = nullptr;
	else {
	    Rf_warning(_("Incompatible methods (\"%s\", \"%s\") for \"%s\""),
		       lname.c_str(), rname.c_str(), generic.c_str());
	    return std::make_pair(0, nullptr);
	}
    }

    S3Launcher* m = (l ? l : r);  // m is the method that will be applied.

    /* we either have a group method or a class method */

    GCStackRoot<Frame> supp_frame(Frame::normalFrame(6));
    // Set up special method bindings:
    m->addMethodBindings(supp_frame);

    if (isOps) {
	// Rebind .Method:
	GCStackRoot<StringVector> dotmethod(StringVector::create(2));
	(*dotmethod)[0] = (l
			   ? const_cast<String*>(l->symbol()->name())
			   : String::blank());
	(*dotmethod)[1] = (r
			   ? const_cast<String*>(r->symbol()->name())
			   : String::blank());
	supp_frame->bind(DotMethodSymbol, dotmethod);
    }

    {
	GCStackRoot<Expression>
	    newcall(new Expression(m->symbol(),
                                   const_cast<PairList*>(call->tail())));
	// Ensure positional matching for operators:
	if (isOps)
            args.stripTags();
	Closure* func = SEXP_downcast<Closure*>(m->function());
	auto ans = newcall->evaluateFunctionCall(func, callenv, args,
                                                 supp_frame);
        return std::make_pair(true, ans);
    }
}

SEXP R_PromiseExpr(SEXP p)
{
    return PRCODE(p);
}

SEXP R_ClosureExpr(SEXP p)
{
    return BODY(p);
}


HIDDEN SEXP do_loadfile(/*const*/ Expression* call, const BuiltInFunction* op, Environment* env, RObject* const* args, int num_args, const PairList* tags)
{
    SEXP file, s;
    FILE *fp;

    PROTECT(file = Rf_coerceVector(args[0], STRSXP));

    if (! Rf_isValidStringF(file))
	Rf_error(_("bad file name"));

    fp = RC_fopen(STRING_ELT(file, 0), "rb", TRUE);
    if (!fp)
	Rf_error(_("unable to open 'file'"));
    s = R_LoadFromFile(fp, 0);
    fclose(fp);

    UNPROTECT(1);
    return s;
}

HIDDEN SEXP do_savefile(/*const*/ Expression* call, const BuiltInFunction* op, Environment* env, RObject* const* args, int num_args, const PairList* tags)
{
    FILE *fp;

    if (!Rf_isValidStringF(args[1]))
	Rf_errorcall(call, _("'file' must be non-empty string"));
    if (TYPEOF(args[2]) != LGLSXP)
	Rf_errorcall(call, _("'ascii' must be logical"));

    fp = RC_fopen(STRING_ELT(args[1], 0), "wb", TRUE);
    if (!fp)
	Rf_error(_("unable to open 'file'"));

    R_SaveToFileV(args[0], fp, INTEGER(args[2])[0], 0);

    fclose(fp);
    return nullptr;
}

HIDDEN SEXP do_setnumthreads(/*const*/ Expression* call, const BuiltInFunction* op, RObject* num_threads_)
{
    int old = R_num_math_threads, newi;
    newi = Rf_asInteger(num_threads_);
    if (newi >= 0 && newi <= R_max_num_math_threads)
	R_num_math_threads = newi;
    return Rf_ScalarInteger(old);
}

HIDDEN SEXP do_setmaxnumthreads(/*const*/ Expression* call, const BuiltInFunction* op, RObject* num_threads_)
{
    int old = R_max_num_math_threads, newi;
    newi = Rf_asInteger(num_threads_);
    if (newi >= 0) {
	R_max_num_math_threads = newi;
	if (R_num_math_threads > R_max_num_math_threads)
	    R_num_math_threads = R_max_num_math_threads;
    }
    return Rf_ScalarInteger(old);
}

/* R_ConstantsRegistry allows runtime detection of modification of compiler
   constants. It is a linked list of weak references. Each weak reference
   refers to a byte-code object (BCODESXPs) as key and to a deep copy of the
   object's constants as value. The head of the list has a nil payload
   instead of a weak reference, stays in the list forever, and is a GC root.*/
static SEXP R_ConstantsRegistry = nullptr;

/* A potentially very verbose report for modified compiler constant. */
static void reportModifiedConstant(SEXP crec, SEXP orig, SEXP copy, int idx)
{
    if (R_check_constants < 5)
	return;

    SEXP consts = VECTOR_ELT(crec, 2);
    int n = LENGTH(consts);
    int i;
    if (idx == -1) {
	for(i = 0; i < n; i++)
	    if (VECTOR_ELT(consts, i) == orig) {
		idx = i;
		break;
	    }
    }
    int oldout = R_OutputCon; /* redirect standard to error output */
    R_OutputCon = 2;
    int oldcheck = R_check_constants; /* guard against recursive invocation */
    R_check_constants = 0;
    if (idx != 0) {
	REprintf(_("ERROR: the modified value of the constant is:\n"));
	Rf_PrintValue(orig);
	REprintf(_("ERROR: the original value of the constant is:\n"));
	Rf_PrintValue(copy);
	REprintf(_("ERROR: the modified constant is at index %d\n"), idx);
	REprintf(_("ERROR: the modified constant is in this function body:\n"));
	Rf_PrintValue(orig);
    } else {
	REprintf(_("ERROR: the modified constant is function body:\n"));
	Rf_PrintValue(orig);
	REprintf(_("ERROR: the body was originally:\n"));
	Rf_PrintValue(copy);
    }
    Rf_findFunctionForBody(VECTOR_ELT(consts, 0));
    R_check_constants = oldcheck;
    R_OutputCon = oldout;
}

/* Checks whether compiler constants linked from the given record
   were modified. */
static Rboolean checkConstantsInRecord(SEXP crec, Rboolean abortOnError)
{
    int i;
    int n = LENGTH(crec);
    Rboolean constsOK = TRUE;

    for (i = 3; i < n;) {
	SEXP corig = VECTOR_ELT(crec, i++);
	SEXP ccopy = VECTOR_ELT(crec, i++);

	/* 39: not numerical comparison, not single NA, not attributes as
           set, do ignore byte-code, do ignore environments of closures,
           not ignore srcref

           srcref is not ignored because ignoring it is expensive
           (it triggers duplication)
        */
	if (!R_compute_identical(corig, ccopy, 39)) {

#ifndef CHECK_ALL_CONSTANTS
	    REprintf(_("ERROR: modification of compiler constant of type %s, length %d\n"), R_CHAR(Rf_type2str(TYPEOF(ccopy))), Rf_length(ccopy));
	    reportModifiedConstant(crec, corig, ccopy, -1);
#else
	    int nc = LENGTH(corig);
	    /* some variables are volatile to prevent the compiler from
	       optimizing them out, for easier debugging */
	    volatile int ci;
	    for(ci = 0; ci < nc; ci++) {
		volatile SEXP orig = VECTOR_ELT(corig, ci);
		volatile SEXP copy = VECTOR_ELT(ccopy, ci);
		if (!R_compute_identical(orig, copy, 39)) {
		    REprintf(_("ERROR: modification of compiler constant of type %s, length %d\n"),
			R_CHAR(Rf_type2str(TYPEOF(copy))), length(copy));
		    reportModifiedConstant(crec, orig, copy, ci);
		}
	    }
#endif
	    constsOK = FALSE;
        }
    }

    if (!constsOK && abortOnError) {
	/* turn off constant checking to avoid infinite recursion through
	   R_Suicide -> ... -> R_RunExitFinalizers -> R_checkConstants. */
	R_check_constants = 0;
	R_Suicide(_("compiler constants were modified!\n"));
    }

    return constsOK;
}

// static void const_cleanup(void *data)
// {
//     Rboolean *inProgress = (Rboolean *)data;
//     *inProgress = FALSE;
// }

/* Checks if constants of any registered BCODESXP have been modified.
   Returns TRUE if the constants are ok, otherwise returns false or aborts.*/
HIDDEN Rboolean R_checkConstants(Rboolean abortOnError)
{
    if (R_check_constants <= 0 || R_ConstantsRegistry == nullptr)
	return TRUE;

    static Rboolean checkingInProgress = FALSE;
    //RCNTXT cntxt;

    if (checkingInProgress)
	/* recursive invocation is possible because of allocation
           in R_compute_identical */
	return TRUE;

    /* set up context to recover checkingInProgress */
    // begincontext(&cntxt, CTXT_CCODE, nullptr, R_BaseEnv, R_BaseEnv,
    //              nullptr, nullptr);
    // cntxt.cend = &const_cleanup;
    // cntxt.cenddata = &checkingInProgress;

    checkingInProgress = TRUE;
    SEXP prev_crec = R_ConstantsRegistry;
    SEXP crec = VECTOR_ELT(prev_crec, 0);
    Rboolean constsOK = TRUE;
    while(crec != nullptr) {
	SEXP wref = VECTOR_ELT(crec, 1);
	SEXP bc = R_WeakRefKey(wref);
	if (!checkConstantsInRecord(crec, abortOnError))
	    constsOK = FALSE;
	if (bc == nullptr)
	    /* remove no longer needed record from the registry */
	    SET_VECTOR_ELT(prev_crec, 0, VECTOR_ELT(crec, 0));
	else
            prev_crec = crec;
	crec = VECTOR_ELT(crec, 0);
    }
    // endcontext(&cntxt);
    checkingInProgress = FALSE;
    return constsOK;
}

HIDDEN SEXP do_returnValue(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return CAR(args); /* default */
}

#include <Parse.h>
SEXP R_ParseEvalString(const char *str, SEXP env)
{
    SEXP s = PROTECT(Rf_mkString(str));

    ParseStatus status;
    SEXP ps = PROTECT(R_ParseVector(s, -1, &status, nullptr));
    if (status != PARSE_OK ||
	TYPEOF(ps) != EXPRSXP ||
	LENGTH(ps) != 1)
	Rf_error("parse error");

    SEXP val = Rf_eval(VECTOR_ELT(ps, 0), env);
    UNPROTECT(2); /* s, ps */
    return val;
}
