/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995--2018  The R Core Team.
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

/** @file errors.cpp
 *
 * Error and warning handling.
 */

#include <signal.h>

// For debugging:
#include <iostream>

#define R_NO_REMAP

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Localization.h>
#include <Internal.h>
/* -> Errormsg.h */
#include <Startup.h> /* rather cleanup ..*/
#include <Rconnections.h>
#include <Rinterface.h>
#include <R_ext/GraphicsEngine.h> /* for GEonExit */
#include <R_ext/Print.h>
#include <cstdarg>

#include <rho/ClosureContext.hpp>
#include <rho/CommandTerminated.hpp>
#include <rho/ConsCell.hpp>
#include <rho/ListVector.hpp>
#include <rho/ReturnException.hpp>
#include <rho/StackChecker.hpp>
#include <rho/strutil.hpp>

using namespace std;
using namespace rho;


#include <R_ext/Visibility.h>


/* Total line length, in chars, before splitting in warnings/errors */
#define LONGWARN 75

/*
Different values of inError are used to indicate different places
in the error handling.
*/
static int inError = 0;
static int inWarning = 0;
static int inPrintWarnings = 0;
static int immediateWarning = 0;
static int noBreakWarning = 0;

static void try_jump_to_restart(void);
// The next is crucial to the use of [[noreturn]] attributes.
[[noreturn]] static void jump_to_top_ex(Rboolean, Rboolean, Rboolean, Rboolean, Rboolean);
static void signalInterrupt(void);
static const char * R_ConciseTraceback(SEXP call, int skip);

/* Interface / Calling Hierarchy :

  R__stop()   -> do_error ->   errorcall --> (eventually) jump_to_top_ex
			 /
		    error

  R__warning()-> do_warning   -> warningcall -> if(warn >= 2) errorcall
			     /
		    warning /

  ErrorMessage()-> errorcall   (but with message from ErrorDB[])

  WarningMessage()-> warningcall (but with message from WarningDB[]).
*/

void R_CheckStack(void)
{
    StackChecker::checkAvailableStackSpace();
}

void R_CheckStack2(size_t extra)
{
    StackChecker::checkAvailableStackSpace(extra);
}

void R_CheckUserInterrupt(void)
{
    R_CheckStack();

    /* Don't do any processing of interrupts, timing limits, or other
       asynchronous events if interrupts are suspended. */
    if (R_interrupts_suspended) return;

    /* This is the point where GUI systems need to do enough event
       processing to determine whether there is a user interrupt event
       pending.  Need to be careful not to do too much event
       processing though: if event handlers written in R are allowed
       to run at this point then we end up with concurrent R
       evaluations and that can cause problems until we have proper
       concurrency support. LT */

    R_ProcessEvents(); /* Also processes timing limits */
    if (R_interrupts_pending) Rf_onintr();

    /* finalizers are run here since this should only be called at
       points where running random code should be sate */
    R_RunPendingFinalizers();
}

static SEXP getInterruptCondition();

static void onintrEx(Rboolean resumeOK)
{
    if (R_interrupts_suspended) {
	R_interrupts_pending = 1;
	return;
    }
    else R_interrupts_pending = 0;

    RObject* hooksym = Rf_install(".signalInterrupt");
    if (SYMVALUE(hooksym) != Symbol::unboundValue()) {
	int resume = FALSE;
	RObject* cond;
	Expression* hcall;
	PROTECT(cond = getInterruptCondition());
	PROTECT(hcall = new Expression(hooksym, PairList::cons(cond, nullptr)));
	resume = Rf_asLogical(hcall->evaluate(Environment::global()));
	UNPROTECT(2);
	if (resume) return;
    }
    else signalInterrupt();

    /* Interrupts do not inherit from error, so we should not run the
       user erro handler. But we have been, so as a transition,
       continue to use options('error') if options('interrupt') is not
       set */
    Rboolean tryUserError = Rboolean(Rf_GetOption1(Rf_install("interrupt")) == nullptr);

    REprintf("\n");
    /* Attempt to save a traceback, show warnings, and reset console;
       also stop at restart (try/browser) frames.  Not clear this is
       what we really want, but this preserves current behavior */
    jump_to_top_ex(TRUE, tryUserError, TRUE, TRUE, FALSE);
}

void Rf_onintr()  { onintrEx(TRUE); }
void Rf_onintrNoResume() { onintrEx(FALSE); }

/* SIGUSR1: save and quit
   SIGUSR2: save and quit, don't run .Last or on.exit().

   These do far more processing than is allowed in a signal handler ....
*/

HIDDEN RETSIGTYPE Rf_onsigusr1(int dummy)
{
    if (R_interrupts_suspended) {
	/**** ought to save signal and handle after suspend */
	REprintf(_("interrupts suspended; signal ignored"));
	signal(SIGUSR1, Rf_onsigusr1);
	return;
    }

    inError = 1;

    if(R_CollectWarnings) Rf_PrintWarnings(nullptr);

    R_ResetConsole();
    R_FlushConsole();
    R_ClearerrConsole();
    R_ParseError = 0;
    R_ParseErrorFile = nullptr;
    R_ParseErrorMsg[0] = '\0';

    /* Bail out if there is a browser/try on the stack--do we really
       want this?  No, as from R 2.4.0
    try_jump_to_restart(); */

    R_CleanUp(SA_SAVE, 2, 1); /* quit, save,  .Last, status=2 */
}


HIDDEN RETSIGTYPE Rf_onsigusr2(int dummy)
{
    inError = 1;

    if (R_interrupts_suspended) {
	/**** ought to save signal and handle after suspend */
	REprintf(_("interrupts suspended; signal ignored"));
	signal(SIGUSR2, Rf_onsigusr2);
	return;
    }

    if(R_CollectWarnings) Rf_PrintWarnings(nullptr);

    R_ResetConsole();
    R_FlushConsole();
    R_ClearerrConsole();
    R_ParseError = 0;
    R_ParseErrorFile = nullptr;
    R_ParseErrorMsg[0] = '\0';
    R_CleanUp(SA_SAVE, 0, 0);
}


static void setupwarnings(void)
{
    R_Warnings = ListVector::create(R_nwarnings);
    Rf_setAttrib(R_Warnings, Symbols::NamesSymbol, Rf_allocVector(STRSXP, R_nwarnings));
}

/* Rvsnprintf: like vsnprintf, but guaranteed to null-terminate. */
#ifdef Win32
int trio_vsnprintf(char *buffer, size_t bufferSize, const char *format,
		   va_list args);

static int Rvsnprintf(char *buf, size_t size, const char  *format, va_list ap)
{
    int val;
    val = trio_vsnprintf(buf, size, format, ap);
    buf[size-1] = '\0';
    return val;
}
#else
static int Rvsnprintf(char *buf, std::size_t size, const char  *format, va_list ap)
{
    int val;
    val = vsnprintf(buf, size, format, ap);
    buf[size-1] = '\0';
    return val;
}
#endif

constexpr size_t BUFSIZE = 8192;

R_INLINE static void RprintTrunc(char *buf)
{
    if(R_WarnLength < BUFSIZE - 20 && strlen(buf) == R_WarnLength) {
	strcat(buf, " ");
	strcat(buf, _("[... truncated]"));
    }
}

static SEXP getCurrentCall()
{
    FunctionContext *c = FunctionContext::innermost();

    /* This can be called before R_GlobalContext is defined, so... */
    /* If profiling is on, this can be a CTXT_BUILTIN */

	return c ? const_cast<Expression*>(c->call()) : static_cast<RObject*>(nullptr);
}


void Rf_warning(const char *format, ...)
{
    char buf[BUFSIZE], *p;

    va_list ap;
    va_start(ap, format);
    Rvsnprintf(buf, min(BUFSIZE, R_WarnLength+1), format, ap);
    va_end(ap);
    p = buf + strlen(buf) - 1;
    if(strlen(buf) > 0 && *p == '\n') *p = '\0';
    RprintTrunc(buf);
    Rf_warningcall(getCurrentCall(), "%s", buf);
}

/* declarations for internal condition handling */

static void vsignalError(SEXP call, const char *format, va_list ap);
static void vsignalWarning(SEXP call, const char *format, va_list ap);
[[noreturn]] static void invokeRestart(SEXP, SEXP);

#include <rlocale.h>

static int wd(const char * buf)
{
    int nc = int(mbstowcs(nullptr, buf, 0)), nw;
    if(nc > 0 && nc < 2000) {
	wchar_t wc[2000];
	mbstowcs(wc, buf, nc + 1);
	nw = Ri18n_wcswidth(wc, 2147483647);
	return (nw < 1) ? nc : nw;
    }
    return nc;
}

static void vwarningcall_dflt(SEXP call, const char *format, va_list ap)
{
    int w;
    SEXP names, s;
    const char *dcall;
    char buf[BUFSIZE];
    ClosureContext *cptr;

    if (inWarning)
	return;

    s = Rf_GetOption1(Rf_install("warning.expression"));
    if( s != nullptr ) {
	if( !Rf_isLanguage(s) &&  ! Rf_isExpression(s) )
	    Rf_error(_("invalid option \"warning.expression\""));
	cptr = R_GlobalContext();
	s->evaluate(cptr->workingEnvironment());
	return;
    }

    w = Rf_asInteger(Rf_GetOption1(Rf_install("warn")));

    if( w == R_NaInt ) /* set to a sensible value */
	w = 0;

    if( w <= 0 && immediateWarning ) w = 1;

    if( w < 0 || inWarning || inError) /* ignore if w<0 or already in here*/
	return;

    inWarning = 1;

    /* use try-catch to restore inWarning if there is an exit */
    try {
    if(w >= 2) { /* make it an error */
	Rvsnprintf(buf, min(BUFSIZE, R_WarnLength), format, ap);
	RprintTrunc(buf);
	inWarning = 0; /* PR#1570 */
	Rf_errorcall(call, _("(converted from warning) %s"), buf);
    }
    else if(w == 1) {	/* print as they happen */
	const char *tr;
	if( call != nullptr ) {
	    dcall = R_CHAR(STRING_ELT(Rf_deparse1s(call), 0));
	} else dcall = "";
	Rvsnprintf(buf, min(BUFSIZE, R_WarnLength+1), format, ap);
	RprintTrunc(buf);

	if(dcall[0] == '\0') REprintf(_("Warning:"));
	else {
	    REprintf(_("Warning in command '%s':"), dcall);
	    if(!(noBreakWarning ||
		 ( mbcslocale && 18 + wd(dcall) + wd(buf) <= LONGWARN) ||
		 (!mbcslocale && 18 + strlen(dcall) + strlen(buf) <= LONGWARN)))
		REprintf("\n ");
	}
	REprintf(" %s\n", buf);
	if(R_ShowWarnCalls && call != nullptr) {
	    tr = R_ConciseTraceback(call, 0);
	    if (strlen(tr)) {REprintf(_("Calls:")); REprintf(" %s\n", tr);}
	}
    }
    else if(w == 0) {	/* collect them */
	if(!R_CollectWarnings) setupwarnings();
	if(R_CollectWarnings < R_nwarnings) {
	    SET_VECTOR_ELT(R_Warnings, R_CollectWarnings, call);
	    Rvsnprintf(buf, min(BUFSIZE, R_WarnLength+1), format, ap);
	    RprintTrunc(buf);
	    if(R_ShowWarnCalls && call != nullptr) {
		const char *tr =  R_ConciseTraceback(call, 0);
		size_t nc = strlen(tr);
		if (nc && nc + (int)strlen(buf) + 8 < BUFSIZE) {
		    strcat(buf, "\n");
		    strcat(buf, _("Calls:"));
		    strcat(buf, " ");
		    strcat(buf, tr);
		}
	    }
	    names = CAR(ATTRIB(R_Warnings));
	    SET_STRING_ELT(names, R_CollectWarnings++, Rf_mkChar(buf));
	}
    }
    /* else:  w <= -1 */
    }
    catch (...) {
	inWarning = 0;
	throw;
    }

    inWarning = 0;
}

static void warningcall_dflt(SEXP call, const char *format,...)
{
    va_list ap;

    va_start(ap, format);
    vwarningcall_dflt(call, format, ap);
    va_end(ap);
}

void Rf_warningcall(SEXP call, const char *format, ...)
{
    va_list ap;
    va_start(ap, format);
    vsignalWarning(call, format, ap);
    va_end(ap);
}

void Rf_warningcall_immediate(SEXP call, const char *format, ...)
{
    va_list ap;

    immediateWarning = 1;
    va_start(ap, format);
    vsignalWarning(call, format, ap);
    va_end(ap);
    immediateWarning = 0;
}

HIDDEN
void Rf_PrintWarnings(const char *hdr = nullptr)
{
    int i;
    const char *header = hdr ? hdr : n_("Warning message:", "Warning messages:", R_CollectWarnings);
    SEXP names;

    if (R_CollectWarnings == 0)
	return;
    else if (inPrintWarnings) {
	if (R_CollectWarnings) {
	    R_CollectWarnings = 0;
	    R_Warnings = nullptr;
	    REprintf(_("Lost warning messages\n"));
	}
	return;
    }

    inPrintWarnings = 1;

    /* use try-catch to restore inPrintWarnings if there is
       an exit */
    try {
    if( R_CollectWarnings == 1 ) {
	REprintf("%s\n", header);
	names = CAR(ATTRIB(R_Warnings));
	if( VECTOR_ELT(R_Warnings, 0) == nullptr )
	    REprintf("%s \n", R_CHAR(STRING_ELT(names, 0)));
	else {
	    const char *dcall, *msg = R_CHAR(STRING_ELT(names, 0));
	    dcall = R_CHAR(STRING_ELT(Rf_deparse1s(VECTOR_ELT(R_Warnings, 0)), 0));
	    REprintf(_("In command '%s':"), dcall);
	    if (mbcslocale) {
		int msgline1;
		const char *p = strchr(msg, '\n');
		if (p) {
		    char *q = const_cast<char*>(p);
		    *q = '\0';
		    msgline1 = wd(msg);
		    *q = '\n';
		} else msgline1 = wd(msg);
		if (6 + wd(dcall) + msgline1 > LONGWARN) REprintf("\n ");
	    } else {
		size_t msgline1 = strlen(msg);
		const char *p = strchr(msg, '\n');
		if (p) msgline1 = (int)(p - msg);
		if (6 + strlen(dcall) + msgline1 > LONGWARN) REprintf("\n ");
	    }
	    REprintf(" %s\n", msg);
	}
    } else if( R_CollectWarnings <= 10 ) {
	REprintf("%s\n", header);
	names = CAR(ATTRIB(R_Warnings));
	for(i = 0; i < R_CollectWarnings; i++) {
	    if( VECTOR_ELT(R_Warnings, i) == nullptr ) {
		REprintf("%d: %s \n", i+1, R_CHAR(STRING_ELT(names, i)));
	    } else {
		const char *dcall, *msg = R_CHAR(STRING_ELT(names, i));
		dcall = R_CHAR(STRING_ELT(Rf_deparse1s(VECTOR_ELT(R_Warnings, i)), 0));
		REprintf("%d: ", i + 1); 
		REprintf(_("In command '%s':"), dcall); 
		if (mbcslocale) {
		    int msgline1;
		    char *p = const_cast<char*>(strchr(msg, '\n'));
		    if (p) {
			*p = '\0';
			msgline1 = wd(msg);
			*p = '\n';
		    } else msgline1 = wd(msg);
		    if (10 + wd(dcall) + msgline1 > LONGWARN) {
			REprintf("\n ");
		    }
		} else {
		    size_t msgline1 = strlen(msg);
		    const char *p = strchr(msg, '\n');
		    if (p) msgline1 = (int)(p - msg);
		    if (10 + strlen(dcall) + msgline1 > LONGWARN) {
			REprintf("\n ");
		    }
		}
		REprintf(" %s\n", msg);
	    }
	}
    } else {
	if (R_CollectWarnings < R_nwarnings)
	    REprintf(n_("There was %d warning (use 'warnings()' to see it)",
			      "There were %d warnings (use 'warnings()' to see them)", 
			      R_CollectWarnings), 
		     R_CollectWarnings);
	else
	    REprintf(_("There were %d or more warnings (use 'warnings()' to see the first %d)"), 
		     R_nwarnings, R_nwarnings);
	REprintf("\n");
    }

	// R_Warnings has names.   Values are calls.
	ListVector* last_warning
	    = ListVector::create(R_Warnings->begin(),
				 R_Warnings->begin() + R_CollectWarnings);
	last_warning->setNames(
	    StringVector::create(R_Warnings->names()->begin(),
				 R_Warnings->names()->begin() + R_CollectWarnings));
	static Symbol* last_warning_symbol = Symbol::obtain("last.warning");
	Environment::base()->frame()->obtainBinding(last_warning_symbol)
	    ->setValue(last_warning);
    }
    catch (...) {
	if (R_CollectWarnings) {
	    R_CollectWarnings = 0;
	    R_Warnings = nullptr;
	    REprintf(_("Lost warning messages\n"));
	}
	inPrintWarnings = 0;
	throw;
    }

    inPrintWarnings = 0;
    R_CollectWarnings = 0;
    R_Warnings = nullptr;
    return;
}

/* Return a constructed source location (e.g. filename#123) from a srcref.  If the srcref
   is not valid "" will be returned.
*/

static SEXP GetSrcLoc(SEXP srcref)
{
    if (TYPEOF(srcref) != INTSXP || Rf_length(srcref) < 4)
	return Rf_ScalarString(Rf_mkChar(""));

    SEXP srcfile = R_GetSrcFilename(srcref);
    static Symbol* basename = Symbol::obtain("basename");
    Expression* e2 = new Expression(basename, { srcfile });
    srcfile = e2->evaluate(Environment::base());
    String* srcfilename = (*SEXP_downcast<StringVector*>(srcfile))[0];

    int line = INTEGER(srcref)[0];

    std::string result = StrCat(srcfilename->stdstring(), '#', line);
    return StringVector::createScalar(String::obtain(result));

}

static char errbuf[BUFSIZE];

#define ERRBUFCAT(txt) strncat(errbuf, txt, BUFSIZE - strlen(errbuf))

const char *R_curErrorBuf() {
    return errbuf;
}

/* temporary hook to allow experimenting with alternate error mechanisms */
static void (*R_ErrorHook)(SEXP, char *) = nullptr;


/* Do not check constants on error more than this number of times per one
   R process lifetime; if so many errors are generated, the performance
   overhead due to the checks would be too high, and the program is doing
   something strange anyway (i.e. running no-segfault tests). The constant
   checks in GC and session exit (or .Call) do not have such limit. */
static int allowedConstsChecks = 1000;

[[noreturn]] static void verrorcall_dflt(SEXP call, const char *format, va_list ap)
{
    if (allowedConstsChecks > 0) {
	allowedConstsChecks--;
	R_checkConstants(TRUE);
    }
    char *p;
    const char *tr;
    int oldInError;

    if (inError) {
	/* fail-safe handler for recursive errors */
	if(inError == 3) {
	     /* Can REprintf generate an error? If so we should guard for it */
	    REprintf(_("Error during wrapup: "));
	    /* this does NOT try to print the call since that could
	       cause a cascade of error calls */
	    Rvsnprintf(errbuf, sizeof(errbuf), format, ap);
	    REprintf("%s\n", errbuf);
	}
	if (R_Warnings != nullptr) {
	    R_CollectWarnings = 0;
	    R_Warnings = nullptr;
	    REprintf(_("Lost warning messages\n"));
	}
	DisableStackCheckingScope scope;
	jump_to_top_ex(FALSE, FALSE, FALSE, FALSE, FALSE);
    }

    oldInError = inError;
    inError = 1;

    /* use try-catch to restore inError value on exit */
    try {
    if(call != nullptr) {
	char tmp[BUFSIZE], tmp2[BUFSIZE];
	const char *head = _("Error in command:"), *tail = "\n  ";
	SEXP srcloc = nullptr; // -Wall
	size_t len = 0;	// indicates if srcloc has been set
	int nprotect = 0, skip = R_NaInt;
	SEXP opt = Rf_GetOption1(Rf_install("show.error.locations"));
	if (!Rf_isNull(opt)) {
	    if (TYPEOF(opt) == STRSXP && Rf_length(opt) == 1) {
	    	if (Rf_pmatch(Rf_ScalarString(Rf_mkChar("top")), opt, FALSE)) skip = 0;
	    	else if (Rf_pmatch(Rf_ScalarString(Rf_mkChar("bottom")), opt, FALSE)) skip = -1;
	    } else if (TYPEOF(opt) == LGLSXP)
	    	skip = Rf_asLogical(opt) == 1 ? 0 : R_NaInt;
	    else
	    	skip = Rf_asInteger(opt);
	}

	const char *dcall = R_CHAR(STRING_ELT(Rf_deparse1s(call), 0));
	snprintf(tmp2, BUFSIZE,  _("Error in command '%s': "), dcall); 
	if (skip != R_NaInt) {
	    PROTECT(srcloc = GetSrcLoc(R_GetCurrentSrcref(skip)));
	    nprotect++;
	    len = strlen(R_CHAR(STRING_ELT(srcloc, 0)));
	    if (len)
		snprintf(tmp2, BUFSIZE,  _("Error in command '%s' (from %s): "), 
			 dcall, R_CHAR(STRING_ELT(srcloc, 0)));
	}

	Rvsnprintf(tmp, min(BUFSIZE, R_WarnLength) - strlen(head), format, ap);
	if (strlen(tmp2) + strlen(tail) + strlen(tmp) < BUFSIZE) {
	    if(len) snprintf(errbuf, BUFSIZE,  
			     _("Error in command '%s' (from %s): "),
			     dcall, R_CHAR(STRING_ELT(srcloc, 0)));
	    else snprintf(errbuf, BUFSIZE,  _("Error in command '%s': "), dcall);
	    if (mbcslocale) {
		int msgline1;
		char *p = strchr(tmp, '\n');
		if (p) {
		    *p = '\0';
		    msgline1 = wd(tmp);
		    *p = '\n';
		} else msgline1 = wd(tmp);
		if (14 + wd(dcall) + msgline1 > LONGWARN)
		    ERRBUFCAT(tail);
	    } else {
		size_t msgline1 = strlen(tmp);
		char *p = strchr(tmp, '\n');
		if (p) msgline1 = int(p - tmp);
		if (14 + strlen(dcall) + msgline1 > LONGWARN)
		    ERRBUFCAT(tail);
	    }
	    ERRBUFCAT(tmp);
	} else {
	    snprintf(errbuf, BUFSIZE, _("Error: "));
	    ERRBUFCAT(tmp); // FIXME
	}
	UNPROTECT(nprotect);
    }
    else {
	snprintf(errbuf, BUFSIZE, _("Error: "));
	p = errbuf + strlen(errbuf);
	Rvsnprintf(p, min(BUFSIZE, R_WarnLength) - strlen(errbuf), format, ap);
    }

    size_t nc = strlen(errbuf);
    if (nc == BUFSIZE - 1) {
	errbuf[BUFSIZE - 4] = '.';
	errbuf[BUFSIZE - 3] = '.';
	errbuf[BUFSIZE - 2] = '.';
	errbuf[BUFSIZE - 1] = '\n';
    }
    else {
	p = errbuf + nc - 1;
	if(*p != '\n') ERRBUFCAT("\n");
    }

    if(R_ShowErrorCalls && call != nullptr) {  /* assume we want to avoid deparse */
	tr = R_ConciseTraceback(call, 0);
	size_t nc = strlen(tr);
	if (nc && nc + strlen(errbuf) + 8 < BUFSIZE) {
	    ERRBUFCAT(_("Calls:"));
	    ERRBUFCAT(" ");
	    ERRBUFCAT(tr);
	    ERRBUFCAT("\n");
	}
    }
    if (R_ShowErrorMessages) REprintf("%s", errbuf);

    if( R_ShowErrorMessages && R_CollectWarnings ) {
	Rf_PrintWarnings(n_("Additional warning message:", "Additional warning messages:", R_CollectWarnings));
    }

	DisableStackCheckingScope scope;
    jump_to_top_ex(TRUE, TRUE, TRUE, TRUE, FALSE);
    }
    catch (...) {
	inError = oldInError;
	throw;
    }
    inError = oldInError;
}

[[noreturn]] static void errorcall_dflt(SEXP call, const char *format,...)
{
    va_list ap;

    va_start(ap, format);
    verrorcall_dflt(call, format, ap);
    va_end(ap);
}

NORET void Rf_errorcall(SEXP call, const char *format,...)
{
    va_list ap;

    va_start(ap, format);
    vsignalError(call, format, ap);
    va_end(ap);

    if (R_ErrorHook != nullptr) {
	char buf[BUFSIZE];
	void (*hook)(SEXP, char *) = R_ErrorHook;
	R_ErrorHook = nullptr; /* to avoid recursion */
	va_start(ap, format);
	Rvsnprintf(buf, min(BUFSIZE, R_WarnLength), format, ap);
	va_end(ap);
	hook(call, buf);
    }

    va_start(ap, format);
    verrorcall_dflt(call, format, ap);
    va_end(ap);
}

/* Like errorcall, but copies all data for the error message into a buffer
   before doing anything else. */
NORET HIDDEN void errorcall_cpy(SEXP call, const char* format, ...)
{
    char buf[BUFSIZE];

    va_list ap;
    va_start(ap, format);
    Rvsnprintf(buf, BUFSIZE, format, ap);
    va_end(ap);

    Rf_errorcall(call, "%s", buf);
}

// geterrmessage(): Return (the global) 'errbuf' as R string
HIDDEN SEXP do_geterrmessage(/*const*/ Expression* call, const BuiltInFunction* op)
{
    return Rf_ScalarString(Rf_mkChar(errbuf));
}

void Rf_error(const char *format, ...)
{
    char buf[BUFSIZE];

    va_list ap;
    va_start(ap, format);
    Rvsnprintf(buf, min(BUFSIZE, R_WarnLength), format, ap);
    va_end(ap);
    Rf_errorcall(getCurrentCall(), "%s", buf);
}

static void try_jump_to_restart(void)
{
    SEXP list;

    for (list = R_RestartStack; list != nullptr; list = CDR(list)) {
	SEXP restart = CAR(list);
	if (TYPEOF(restart) == VECSXP && LENGTH(restart) > 1) {
	    SEXP name = VECTOR_ELT(restart, 0);
	    if (TYPEOF(name) == STRSXP && LENGTH(name) == 1) {
		const char *cname = R_CHAR(STRING_ELT(name, 0));
		if (! strcmp(cname, "browser") ||
		    ! strcmp(cname, "tryRestart") ||
		    ! strcmp(cname, "abort")) /**** move abort eventually? */
		    invokeRestart(restart, nullptr);
	    }
	}
    }
}

static void jump_to_top_ex(Rboolean traceback,
			   Rboolean tryUserHandler,
			   Rboolean processWarnings,
			   Rboolean resetConsole,
			   Rboolean ignoreRestartContexts)
{
    SEXP s;
    int haveHandler, oldInError;

    oldInError = inError;
    haveHandler = FALSE;

    /* use try-catch to restore inError value on exit */
    try {
	if (tryUserHandler && inError < 3) {
	    if (! inError)
		inError = 1;

	    /* now see if options("error") is set */
	    s = Rf_GetOption1(Rf_install("error"));
	    haveHandler = ( s != nullptr );
	    if (haveHandler) {
		if( !Rf_isLanguage(s) &&  ! Rf_isExpression(s) )  /* shouldn't happen */
		    REprintf(_("invalid option \"error\"\n"));
		else {
		    inError = 3;
		    if (Rf_isLanguage(s))
			s->evaluate(Environment::global());
		    else /* expression */
			{
			    int i, n = LENGTH(s);
			    for (i = 0 ; i < n ; i++)
				Rf_eval(XVECTOR_ELT(s, i), R_GlobalEnv);
			}
		    inError = oldInError;
		}
	    }
	    inError = oldInError;
	}

	/* print warnings if there are any left to be printed */
	if( processWarnings && R_CollectWarnings )
	    Rf_PrintWarnings(nullptr);

	/* reset some stuff--not sure (all) this belongs here */
	if (resetConsole) {
	    R_ResetConsole();
	    R_FlushConsole();
	    R_ClearerrConsole();
	    R_ParseError = 0;
	    R_ParseErrorFile = nullptr;
	    R_ParseErrorMsg[0] = '\0';
	}

	/*
	 * Reset graphics state
	 */
	GEonExit();

	/* WARNING: If oldInError > 0 ABSOLUTELY NO ALLOCATION can be
	   triggered after this point except whatever happens in writing
	   the traceback and R_run_onexits.  The error could be an out of
	   memory error and any allocation could result in an
	   infinite-loop condition. All you can do is reset things and
	   exit.  */

	/* jump to a browser/try if one is on the stack */
	if (!ignoreRestartContexts) {
	    try_jump_to_restart();
	}
	/* at this point, i.e. if we have not exited in
	   try_jump_to_restart, we are heading for top level */

	    /* write traceback if requested, unless we're already doing it
	       or there is an inconsistency between inError and oldInError
	       (which should not happen) */
	    if (traceback && inError < 2 && inError == oldInError) {
		inError = 2;
		PROTECT(s = R_GetTraceback(0));
		SET_SYMVALUE(Rf_install(".Traceback"), s);
		/* should have been defineVar
		   setVar(Rf_install(".Traceback"), s, R_GlobalEnv); */
		UNPROTECT(1);
		inError = oldInError;
	}

	throw CommandTerminated();
    }
    catch (...) {
	inError = oldInError;
	throw;
    }

    /* not reached
       inError = oldInError;
    */
}

NORET void jump_to_toplevel()
{
    /* no traceback, no user error option; for now, warnings are
       printed here and console is reset -- eventually these should be
       done after arriving at the jump target.  Now ignores
       try/browser frames--it really is a jump to toplevel */
    jump_to_top_ex(FALSE, FALSE, TRUE, TRUE, TRUE);
}

/* #define DEBUG_GETTEXT 1 */

/* gettext(domain, string) */
HIDDEN SEXP do_gettext(/*const*/ Expression* call, const BuiltInFunction* op, RObject* dots_, RObject* domain_)
{
#ifdef ENABLE_NLS
    const char *domain = "", *cfn;
    char *buf;
    SEXP ans, string = domain_;
    int i, n = LENGTH(string);

    if(Rf_isNull(string) || !n) return string;

    if(!Rf_isString(string)) Rf_error(_("invalid '%s' value"), "string");

    if(Rf_isNull(dots_)) {
	ClosureContext *cptr
	    = ClosureContext::innermost(Evaluator::Context::innermost()->nextOut());
	SEXP rho = R_BaseEnv;
	for (;
	     cptr != nullptr;
	     cptr = ClosureContext::innermost(cptr->nextOut())) {
	    /* stop() etc have internal call to .makeMessage */
	    cfn = R_CHAR(STRING_ELT(Rf_deparse1s(CAR(const_cast<Expression*>(cptr->call()))), 0));
	    if(streql(cfn, "stop") || streql(cfn, "warning")
	       || streql(cfn, "message")) continue;
	    rho = cptr->workingEnvironment();
	    // You might think a break was intended at this point, but
	    // Brian Ripley assures us not: PR14367.
	}
	while(rho != R_EmptyEnv) {
	    if (rho == R_GlobalEnv) break;
	    else if (R_IsNamespaceEnv(rho)) {
		domain = Rf_translateChar(STRING_ELT(R_NamespaceEnvSpec(rho), 0));
		break;
	    }
	    rho = ENCLOS(rho);
	}
	if(strlen(domain)) {
	    size_t len = strlen(domain)+3;
	    R_CheckStack2(len);
	    buf = static_cast<char *>(alloca(len));
	    snprintf(buf, len, "R-%s", domain);
	    domain = buf;
	}
    } else if(Rf_isString(dots_))
	domain = Rf_translateChar(STRING_ELT(dots_,0));
    else if(Rf_isLogical(dots_) && LENGTH(dots_) == 1 && LOGICAL(dots_)[0] == R_NaLog) ;
    else Rf_error(_("invalid '%s' value"), "domain");

    if(strlen(domain)) {
	PROTECT(ans = Rf_allocVector(STRSXP, n));
	for(i = 0; i < n; i++) {
	    int ihead = 0, itail = 0;
	    const char * This = Rf_translateChar(STRING_ELT(string, i));
	    char *tmp, *head = nullptr, *tail = nullptr, *p, *tr;
	    R_CheckStack2(strlen(This) + 1);
	    tmp = static_cast<char *>(alloca(strlen(This) + 1));
	    strcpy(tmp, This);
	    /* strip leading and trailing white spaces and
	       add back after translation */
	    for(p = tmp;
		*p && (*p == ' ' || *p == '\t' || *p == '\n');
		p++, ihead++) ;
	    if(ihead > 0) {
		R_CheckStack2(ihead + 1);
		head = static_cast<char *>(alloca(ihead + 1));
		strncpy(head, tmp, ihead);
		head[ihead] = '\0';
		tmp += ihead;
		}
	    if(strlen(tmp))
		for(p = tmp+strlen(tmp)-1;
		    p >= tmp && (*p == ' ' || *p == '\t' || *p == '\n');
		    p--, itail++) ;
	    if(itail > 0) {
		R_CheckStack2(itail + 1);
		tail = static_cast<char *>(alloca(itail + 1));
		strcpy(tail, tmp+strlen(tmp)-itail);
		tmp[strlen(tmp)-itail] = '\0';
	    }
	    if(strlen(tmp)) {
#ifdef DEBUG_GETTEXT
		REprintf("translating '%s' in domain '%s'\n", tmp, domain);
#endif
		tr = dgettext(domain, tmp);
		R_CheckStack2(strlen(tr) + ihead + itail + 1);
		tmp = static_cast<char *>(alloca(strlen(tr) + ihead + itail + 1));
		tmp[0] ='\0';
		if(ihead > 0) strcat(tmp, head);
		strcat(tmp, tr);
		if(itail > 0) strcat(tmp, tail);
		SET_STRING_ELT(ans, i, Rf_mkChar(tmp));
	    } else
		SET_STRING_ELT(ans, i, Rf_mkChar(This));
	}
	UNPROTECT(1);
	return ans;
    } else return domain_;
#else
    return CADR(args);
#endif
}

/* ngettext(n, msg1, msg2, domain) */
HIDDEN SEXP do_ngettext(/*const*/ Expression* call, const BuiltInFunction* op, RObject* n_, RObject* msg1_, RObject* msg2_, RObject* domain_)
{
#ifdef ENABLE_NLS
    const char *domain = "", *cfn;;
    char *buf;
    SEXP ans, sdom = domain_;
#endif
    SEXP msg1 = msg1_, msg2 = msg2_;
    int n = Rf_asInteger(n_);

    if(n == R_NaInt || n < 0) Rf_error(_("invalid '%s' argument"), "n");
    if(!Rf_isString(msg1) || LENGTH(msg1) != 1)
	Rf_error(_("'%s' must be a character string"), "msg1");
    if(!Rf_isString(msg2) || LENGTH(msg2) != 1)
	Rf_error(_("'%s' must be a character string"), "msg2");

#ifdef ENABLE_NLS
    if(Rf_isNull(sdom)) {
	Environment* rho = Environment::empty();

	for(ClosureContext *cptr = ClosureContext::innermost(
		Evaluator::Context::innermost()->nextOut());
	    cptr != nullptr;
	    cptr = ClosureContext::innermost(cptr->nextOut()))
	{
	    /* stop() etc have internal call to .makeMessage */
	    cfn = R_CHAR(STRING_ELT(Rf_deparse1s(cptr->call()->car()), 0));
		if(streql(cfn, "stop") || streql(cfn, "warning")
		   || streql(cfn, "message")) continue;
		rho = cptr->workingEnvironment();
	}
	while(rho != R_EmptyEnv) {
	    if (rho == R_GlobalEnv) break;
	    else if (R_IsNamespaceEnv(rho)) {
		domain = Rf_translateChar(STRING_ELT(R_NamespaceEnvSpec(rho), 0));
		break;
	    }
	    rho = rho->enclosingEnvironment();
	}
	if(strlen(domain)) {
	    size_t len = strlen(domain)+3;
	    R_CheckStack2(len);
	    buf = static_cast<char *>(alloca(len));
	    snprintf(buf, len, "R-%s", domain);
	    domain = buf;
	}
    } else if(Rf_isString(sdom))
	domain = R_CHAR(STRING_ELT(sdom,0));
    else if(Rf_isLogical(sdom) && LENGTH(sdom) == 1 && LOGICAL(sdom)[0] == R_NaLog) ;
    else Rf_error(_("invalid '%s' value"), "domain");

    /* libintl seems to malfunction if given a message of "" */
    if(strlen(domain) && Rf_length(STRING_ELT(msg1, 0))) {
	char *fmt = dngettext(domain,
			      Rf_translateChar(STRING_ELT(msg1, 0)),
			      Rf_translateChar(STRING_ELT(msg2, 0)),
			      n);
	PROTECT(ans = Rf_mkString(fmt));
	UNPROTECT(1);
	return ans;
    } else
#endif
	return n == 1 ? msg1 : msg2;
}


/* bindtextdomain(domain, dirname) */
HIDDEN SEXP do_bindtextdomain(/*const*/ Expression* call, const BuiltInFunction* op, RObject* domain_, RObject* dirname_)
{
#ifdef ENABLE_NLS
    char *res;

    if(!Rf_isString(domain_) || LENGTH(domain_) != 1)
	Rf_error(_("invalid '%s' value"), "domain");
    if(Rf_isNull(dirname_)) {
	res = bindtextdomain(Rf_translateChar(STRING_ELT(domain_,0)), nullptr);
    } else {
	if(!Rf_isString(dirname_) || LENGTH(dirname_) != 1)
	    Rf_error(_("invalid '%s' value"), "dirname");
	res = bindtextdomain(Rf_translateChar(STRING_ELT(domain_,0)),
			     Rf_translateChar(STRING_ELT(dirname_,0)));
    }
    if(res) return Rf_mkString(res);
    /* else this failed */
#endif
    return nullptr;
}

static Expression* findCall(void)
{
    ClosureContext *cptr
	= ClosureContext::innermost(ClosureContext::innermost()->nextOut());
    return (cptr ? const_cast<Expression*>(cptr->call()) : nullptr);
}

NORET HIDDEN SEXP do_stop(SEXP call, SEXP op, SEXP args, SEXP rho)
{
/* Rf_error(.) : really doesn't return anything; but all do_foo() must be SEXP */
    Expression* c_call;
    checkArity(op, args);

    if(Rf_asLogical(CAR(args))) /* find context -> "Error in ..:" */
	c_call = findCall();
    else
	c_call = nullptr;

    args = CDR(args);

    if (CAR(args) != nullptr) { /* message */
      SETCAR(args, Rf_coerceVector(CAR(args), STRSXP));
      if(!Rf_isValidString(CAR(args)))
	  Rf_errorcall(c_call, _(" [invalid string in stop(.)]"));
      Rf_errorcall(c_call, "%s", Rf_translateChar(STRING_ELT(CAR(args), 0)));
    }
    else
      Rf_errorcall(c_call, "");
    /* never called: */
}

HIDDEN SEXP do_warning(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP c_call;
    checkArity(op, args);

    if(Rf_asLogical(CAR(args))) /* find context -> "... in: ..:" */
	c_call = findCall();
    else
	c_call = nullptr;

    args = CDR(args);
    if(Rf_asLogical(CAR(args))) { /* immediate = TRUE */
	immediateWarning = 1;
    } else
	immediateWarning = 0;
    args = CDR(args);
    if(Rf_asLogical(CAR(args))) { /* noBreak = TRUE */
	noBreakWarning = 1;
    } else
	noBreakWarning = 0;
    args = CDR(args);
    if (CAR(args) != nullptr) {
	SETCAR(args, Rf_coerceVector(CAR(args), STRSXP));
	if(!Rf_isValidString(CAR(args)))
	    Rf_warningcall(c_call, _(" [invalid string in Rf_warning(.)]"));
	else
	    Rf_warningcall(c_call, "%s", Rf_translateChar(STRING_ELT(CAR(args), 0)));
    }
    else
	Rf_warningcall(c_call, "");
    immediateWarning = 0; /* reset to internal calls */
    noBreakWarning = 0;

    return CAR(args);
}

/* Error recovery for incorrect argument count error. */
NORET HIDDEN void WrongArgCount(const char* s)
{
    Rf_error(_("incorrect number of arguments to \"%s\""), s);
}


NORET void UNIMPLEMENTED(const char *s)
{
    Rf_error(_("unimplemented feature in %s"), s);
}

/* ERROR_.. codes in Errormsg.h */
static struct ErrorDB {
    R_ERROR code;
    const char* format;
}
const ErrorDB[] = {
    { ERROR_NUMARGS,		N_("invalid number of arguments")	},
    { ERROR_ARGTYPE,		N_("invalid argument type")		},

    { ERROR_TSVEC_MISMATCH,	N_("time-series/vector length mismatch")},
    { ERROR_INCOMPAT_ARGS,	N_("incompatible arguments")		},

    { ERROR_UNIMPLEMENTED,	N_("unimplemented feature in %s")	},
    { ERROR_UNKNOWN,		N_("unknown error (report this!)")	}
};

static struct WarningDB {
    R_WARNING code;
    const char* format;
}
WarningDB[] = {
    { WARNING_coerce_NA,	N_("NAs introduced by coercion")	},
    { WARNING_coerce_INACC,	N_("inaccurate integer conversion in coercion")},
    { WARNING_coerce_IMAG,	N_("imaginary parts discarded in coercion") },

    { WARNING_UNKNOWN,		N_("unknown warning (report this!)")	},
};


NORET HIDDEN void Rf_ErrorMessage(SEXP call, int which_error, ...)
{
    int i;
    char buf[BUFSIZE];
    va_list ap;

    i = 0;
    while(ErrorDB[i].code != ERROR_UNKNOWN) {
	if (ErrorDB[i].code == which_error)
	    break;
	i++;
    }

    va_start(ap, which_error);
    Rvsnprintf(buf, BUFSIZE, _(ErrorDB[i].format), ap);
    va_end(ap);
    Rf_errorcall(call, "%s", buf);
}

HIDDEN
void WarningMessage(SEXP call, int /* R_WARNING */ which_warn, ...)
{
    int i;
    char buf[BUFSIZE];
    va_list ap;

    i = 0;
    while(WarningDB[i].code != WARNING_UNKNOWN) {
	if (WarningDB[i].code == which_warn)
	    break;
	i++;
    }

/* clang pre-3.9.0 says
      warning: passing an object that undergoes default argument promotion to
      'va_start' has undefined behavior [-Wvarargs]
*/
    va_start(ap, which_warn);
    Rvsnprintf(buf, BUFSIZE, _(WarningDB[i].format), ap);
    va_end(ap);
    Rf_warningcall(call, "%s", buf);
}

#ifdef UNUSED
/* temporary hook to allow experimenting with alternate warning mechanisms */
static void (*R_WarningHook)(SEXP, char *) = nullptr;

void R_SetWarningHook(void (*hook)(SEXP, char *))
{
    R_WarningHook = hook;
}

void R_SetErrorHook(void (*hook)(SEXP, char *))
{
    R_ErrorHook = hook;
}
#endif

static void R_SetErrmessage(const char *s)
{
    strncpy(errbuf, s, sizeof(errbuf));
    errbuf[sizeof(errbuf) - 1] = 0;
}

static void R_PrintDeferredWarnings(void)
{
    if( R_ShowErrorMessages && R_CollectWarnings ) {
	Rf_PrintWarnings(n_("Additional warning message:", "Additional warning messages:", R_CollectWarnings));
    }
}

HIDDEN
SEXP R_GetTraceback(int skip)
{
    int nback = 0, ns;
    FunctionContext *c;
    SEXP s, t;

    for (c = FunctionContext::innermost(), ns = skip;
	 c != nullptr;
	 c = FunctionContext::innermost(c->nextOut()))
	if (ns > 0)
	    ns--;
	else
	    nback++;

    PROTECT(s = Rf_allocList(nback));
    t = s;
    for (c = FunctionContext::innermost() ;
	 c != nullptr;
	 c = FunctionContext::innermost(c->nextOut()))
	if (skip > 0)
	    skip--;
	else {
	    SETCAR(t, Rf_deparse1m(const_cast<Expression*>(c->call()), FALSE, DEFAULTDEPARSE));
	    if (c->sourceLocation() && !Rf_isNull(c->sourceLocation())) 
		Rf_setAttrib(CAR(t), Symbols::SrcrefSymbol, Rf_duplicate(c->sourceLocation()));
	    t = CDR(t);
	}
    UNPROTECT(1);
    return s;
}

HIDDEN SEXP do_traceback(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_)
{
    int skip;

    skip = Rf_asInteger(x_);

    if (skip == R_NaInt || skip < 0 )
	Rf_error(_("invalid '%s' value"), "skip");

    return R_GetTraceback(skip);
}

// Utility intended to be called from a debugger.  Prints out the
// hierarchy of R function calls, as recorded by FunctionContexts.
namespace rho {
    void TRACEBACK()
    {
	GCManager::GCInhibitor gci;
	GCStackRoot<PairList> tb(SEXP_downcast<PairList*>(R_GetTraceback(0)));
	while (tb) {
	    StringVector* sv = SEXP_downcast<StringVector*>(tb->car());
	    for (unsigned int i = 0; i < sv->size(); ++i) {
		cout << (i == 0 ? "* " : "  ");
		cout << (*sv)[i]->c_str() << '\n';
	    }
	    tb = tb->tail();
	}
    }
}

static const char * R_ConciseTraceback(SEXP call, int skip)
{
    static char buf[560];
    FunctionContext *c;
    size_t nl;
    int ncalls = 0;
    Rboolean too_many = FALSE;
    const char *top = "" /* -Wall */;

    buf[0] = '\0';
    for (c = FunctionContext::innermost();
	 c != nullptr;
	 c = FunctionContext::innermost(c->nextOut()))
	if (skip > 0)
	    skip--;
	else {
	    SEXP fun = CAR(const_cast<Expression*>(c->call()));
	    const char *funstr = (TYPEOF(fun) == SYMSXP) ?
		R_CHAR(PRINTNAME(fun)) : "<Anonymous>";
	    if(streql(funstr, "stop") ||
	       streql(funstr, "warning") ||
	       streql(funstr, "suppressWarnings") ||
	       streql(funstr, ".signalSimpleWarning")) {
		buf[0] =  '\0'; ncalls = 0; too_many = FALSE;
	    } else {
		ncalls++;
		if(too_many) {
		    top = funstr;
		} else if(int(strlen(buf)) > R_NShowCalls) {
		    memmove(buf+4, buf, strlen(buf)+1);
		    memcpy(buf, "... ", 4);
		    too_many = TRUE;
		    top = funstr;
		} else if(strlen(buf)) {
		    nl = strlen(funstr);
		    memmove(buf+nl+4, buf, strlen(buf)+1);
		    memcpy(buf, funstr, strlen(funstr));
		    memcpy(buf+nl, " -> ", 4);
		} else
		    memcpy(buf, funstr, strlen(funstr)+1);
	    }
	}
    if(too_many && (nl = strlen(top)) < 50) {
	memmove(buf+nl+1, buf, strlen(buf)+1);
	memcpy(buf, top, strlen(top));
	memcpy(buf+nl, " ", 1);
    }
    /* don't add Calls if it adds no extra information */
    /* However: do we want to include the call in the list if it is a
       primitive? */
    if (ncalls == 1 && TYPEOF(call) == LANGSXP) {
	SEXP fun = CAR(call);
	const char *funstr = (TYPEOF(fun) == SYMSXP) ?
	    R_CHAR(PRINTNAME(fun)) : "<Anonymous>";
	if(streql(buf, funstr)) return "";
    }
    return buf;
}

namespace
{
	struct HandlerEntry : RObject
	{
		GCEdge<String> m_class;
		GCEdge<Environment> m_parent_environment;
		GCEdge<> m_handler;
		GCEdge<Environment> m_environment;
		GCEdge<ListVector> m_result;
		bool m_calling;

		HandlerEntry(String *the_class, Environment *parent_env,
					 RObject *handler, Environment *environment,
					 ListVector *result, bool calling)
			: m_calling(calling)
		{
			m_class = the_class;
			m_parent_environment = parent_env;
			m_handler = handler;
			m_environment = environment;
			m_result = result;
		}

		static const char *staticTypeName()
		{
			return "(error handler entry)";
		}

		// Virtual functions of GCNode:
		void detachReferents() override;
		void visitReferents(const_visitor *v) const override;
	};

	void HandlerEntry::detachReferents()
	{
		m_class.detach();
		m_parent_environment.detach();
		m_handler.detach();
		m_environment.detach();
		m_result.detach();
		RObject::detachReferents();
	}

	void HandlerEntry::visitReferents(const_visitor *v) const
	{
		const GCNode *cl = m_class;
		const GCNode *parenv = m_parent_environment;
		const GCNode *handler = m_handler;
		const GCNode *env = m_environment;
		const GCNode *result = m_result;
		RObject::visitReferents(v);
		if (cl)
			(*v)(cl);
		if (parenv)
			(*v)(parenv);
		if (handler)
			(*v)(handler);
		if (env)
			(*v)(env);
		if (result)
			(*v)(result);
	}
}

static SEXP mkHandlerEntry(SEXP klass, SEXP parentenv, SEXP handler, SEXP rho,
						   SEXP result, int calling)
{
	HandlerEntry *entry = new HandlerEntry(SEXP_downcast<String *>(klass),
										   SEXP_downcast<Environment *>(parentenv), handler,
										   SEXP_downcast<Environment *>(rho),
										   SEXP_downcast<ListVector *>(result),
										   (calling != 0));
	return entry;
}

static void push_handler(SEXP handlerEntry) {
    R_HandlerStack = PairList::cons(handlerEntry, R_HandlerStack);
}
static void push_restart(SEXP restart) {
    R_RestartStack = PairList::cons(restart, R_RestartStack);
}
static void pop_restart() {
    R_RestartStack = R_RestartStack->tail();
}

namespace {
    /**** rename these??*/
    bool IS_CALLING_ENTRY(SEXP e)
    {
	return SEXP_downcast<HandlerEntry*>(e)->m_calling;
    }

    String* ENTRY_CLASS(SEXP e)
    {
	return SEXP_downcast<HandlerEntry*>(e)->m_class;
    }

    RObject* ENTRY_HANDLER(SEXP e)
    {
	return SEXP_downcast<HandlerEntry*>(e)->m_handler;
    }

    Environment* ENTRY_TARGET_ENVIR(SEXP e)
    {
	return SEXP_downcast<HandlerEntry*>(e)->m_environment;
    }

    ListVector* ENTRY_RETURN_RESULT(SEXP e)
    {
	return SEXP_downcast<HandlerEntry*>(e)->m_result;
    }
}

#define RESULT_SIZE 4

static SEXP R_HandlerResultToken = nullptr;

HIDDEN void R_FixupExitingHandlerResult(SEXP result)
{
    /* The internal error handling mechanism stores the error message
       in 'errbuf'.  If an on.exit() action is processed while jumping
       to an exiting handler for such an error, then endcontext()
       calls R_FixupExitingHandlerResult to save the error message
       currently in the buffer before processing the on.exit
       action. This is in case an error occurs in the on.exit action
       that over-writes the buffer. The allocation should occur in a
       more favorable stack context than before the jump. The
       R_HandlerResultToken is used to make sure the result being
       modified is associated with jumping to an exiting handler. */
    if (result != nullptr &&
	TYPEOF(result) == VECSXP &&
	XLENGTH(result) == RESULT_SIZE &&
	VECTOR_ELT(result, 0) == nullptr &&
	VECTOR_ELT(result, RESULT_SIZE - 1) == R_HandlerResultToken) {
	SET_VECTOR_ELT(result, 0, Rf_mkString(errbuf));
    }
}

HIDDEN SEXP do_addCondHands(/*const*/ Expression* call, const BuiltInFunction* op, RObject* classes, RObject* handlers, RObject* parentenv, RObject* target, RObject* calling_)
{
    int calling = Rf_asLogical(calling_);

    if (R_HandlerResultToken == nullptr) {
	R_HandlerResultToken = Rf_allocVector(VECSXP, 1);
	R_PreserveObject(R_HandlerResultToken);
    }

    if (classes == nullptr || handlers == nullptr)
	return R_HandlerStack;

    if (TYPEOF(classes) != STRSXP || TYPEOF(handlers) != VECSXP ||
	LENGTH(classes) != LENGTH(handlers))
	Rf_error(_("bad handler data"));

    int n = LENGTH(handlers);
    SEXP oldstack = R_HandlerStack;

    SEXP result = Rf_allocVector(VECSXP, RESULT_SIZE);
    SET_VECTOR_ELT(result, RESULT_SIZE - 1, R_HandlerResultToken);

    for (int i = n - 1; i >= 0; i--) {
	SEXP klass = STRING_ELT(classes, i);
	SEXP handler = VECTOR_ELT(handlers, i);
	SEXP entry = mkHandlerEntry(klass, parentenv, handler, target, result,
				    calling);
	push_handler(entry);
    }

    return oldstack;
}

static PairList* findSimpleErrorHandler(void)
{
    for (PairList& item : *R_HandlerStack) {
	SEXP entry = item.car();
	if (! strcmp(R_CHAR(ENTRY_CLASS(entry)), "simpleError") ||
	    ! strcmp(R_CHAR(ENTRY_CLASS(entry)), "error") ||
	    ! strcmp(R_CHAR(ENTRY_CLASS(entry)), "condition"))
	    return &item;
    }
    return nullptr;
}

static void vsignalWarning(SEXP call, const char *format, va_list ap)
{
    char buf[BUFSIZE];

    static Symbol* hooksym = Symbol::obtain(".signalSimpleWarning");
    if (SYMVALUE(hooksym) != R_UnboundValue &&
	SYMVALUE(Symbols::QuoteSymbol) != R_UnboundValue)
    {
	Expression* qcall = new Expression(Symbols::QuoteSymbol, { call });
	Rvsnprintf(buf, BUFSIZE - 1, format, ap);
	Expression* hcall = new Expression(hooksym, { Rf_mkString(buf), qcall });
	hcall->evaluate(Environment::global());
    }
    else vwarningcall_dflt(call, format, ap);
}

[[noreturn]] static void gotoExitingHandler(SEXP cond, SEXP call, SEXP entry)
{
    SEXP rho = ENTRY_TARGET_ENVIR(entry);
    SEXP result = ENTRY_RETURN_RESULT(entry);
    SET_VECTOR_ELT(result, 0, cond);
    SET_VECTOR_ELT(result, 1, call);
    SET_VECTOR_ELT(result, 2, ENTRY_HANDLER(entry));
    Environment* envir = SEXP_downcast<Environment*>(rho);
    if (!envir->canReturn())
	Rf_error(_("no function to return from, jumping to top level"));
    throw ReturnException(envir, result);
}

static void vsignalError(SEXP call, const char *format, va_list ap)
{
    char localbuf[BUFSIZE];
    PairList* list;

    GCStackRoot<PairList> oldstack(R_HandlerStack);
    Rvsnprintf(localbuf, BUFSIZE - 1, format, ap);
    while ((list = findSimpleErrorHandler()) != nullptr) {
	char *buf = errbuf;
	SEXP entry = list->car();
	R_HandlerStack = list->tail();
	strncpy(buf, localbuf, BUFSIZE);
	/*	Rvsnprintf(buf, BUFSIZE - 1, format, ap);*/
	buf[BUFSIZE - 1] = 0;
	if (IS_CALLING_ENTRY(entry)) {
	    if (!ENTRY_HANDLER(entry))
		return; /* go to default error handling; do not reset stack */
	    else {
		/* protect oldstack here, not outside loop, so handler
		   stack gets unwound in case error is protect stack
		   overflow */
		static Symbol* hooksym = Symbol::obtain(".handleSimpleError");
		Expression* qcall = new Expression(Symbols::QuoteSymbol, { call });
		Expression* hcall = new Expression(
		    hooksym,
		    { ENTRY_HANDLER(entry), Rf_mkString(buf), qcall });
		hcall->evaluate(Environment::global());
	    }
	}
	else gotoExitingHandler(nullptr, call, entry);
    }
    R_HandlerStack = oldstack;
}

static PairList* findConditionHandler(SEXP cond)
{
    SEXP classes = Rf_getAttrib(cond, Symbols::ClassSymbol);

    if (TYPEOF(classes) != STRSXP)
	return nullptr;

    /**** need some changes here to allow conditions to be S4 classes */
    for (PairList& item : *R_HandlerStack) {
	SEXP entry = item.car();
	for (int i = 0; i < LENGTH(classes); i++)
	    if (! strcmp(R_CHAR(ENTRY_CLASS(entry)),
			 R_CHAR(STRING_ELT(classes, i))))
		return &item;
    }
    return nullptr;
}

HIDDEN SEXP do_signalCondition(/*const*/ Expression* call, const BuiltInFunction* op, RObject* cond, RObject* msg, RObject* ecall)
{
    PairList* list;
    GCStackRoot<PairList> oldstack(R_HandlerStack);
    while ((list = findConditionHandler(cond)) != nullptr) {
	SEXP entry = list->car();
	R_HandlerStack = list->tail();
	if (IS_CALLING_ENTRY(entry)) {
	    SEXP h = ENTRY_HANDLER(entry);
	    if (!h) {
		const char *msgstr = nullptr;
		if (TYPEOF(msg) == STRSXP && LENGTH(msg) > 0)
		    msgstr = Rf_translateChar(STRING_ELT(msg, 0));
		else Rf_error(_("error message not a string"));
		errorcall_dflt(ecall, "%s", msgstr);
	    }
	    else {
		Expression* hcall = new Expression(h, { cond });
		hcall->evaluate(Environment::global());
	    }
	}
	else gotoExitingHandler(cond, ecall, entry);
    }
    R_HandlerStack = oldstack;
    return nullptr;
}

static PairList* findInterruptHandler(void)
{
    for (PairList& item : *R_HandlerStack) {
	SEXP entry = item.car();
	if (! strcmp(R_CHAR(ENTRY_CLASS(entry)), "interrupt") ||
	    ! strcmp(R_CHAR(ENTRY_CLASS(entry)), "condition"))
	    return &item;
    }
    return nullptr;
}

static SEXP getInterruptCondition(void)
{
    /**** FIXME: should probably pre-allocate this */
    SEXP cond, klass;
    PROTECT(cond = Rf_allocVector(VECSXP, 0));
    PROTECT(klass = Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(klass, 0, Rf_mkChar("interrupt"));
    SET_STRING_ELT(klass, 1, Rf_mkChar("condition"));
    Rf_classgets(cond, klass);
    UNPROTECT(2);
    return cond;
}

static void signalInterrupt(void)
{
    GCStackRoot<PairList> oldstack(R_HandlerStack);
    PairList* list;
    while ((list = findInterruptHandler()) != nullptr) {
	SEXP entry = list->car();
	R_HandlerStack = list->tail();
	SEXP cond = PROTECT(getInterruptCondition());
	if (IS_CALLING_ENTRY(entry)) {
	    SEXP h = ENTRY_HANDLER(entry);
	    Expression* hcall = new Expression(h, { cond });
	    hcall->evaluate(Environment::global());
	}
	else gotoExitingHandler(cond, nullptr, entry);
	UNPROTECT(1);
    }
    R_HandlerStack = oldstack;

    SEXP h = Rf_GetOption1(Rf_install("interrupt"));
    if (h != nullptr) {
	Expression* call = new Expression(h, { nullptr });
	call->evaluate(Environment::global());
    }
}

HIDDEN void
R_InsertRestartHandlers(ClosureContext *cptr, const char *cname)
{
    SEXP klass, rho, entry, name;

    if (cptr->handlerStack() != R_HandlerStack)
	Rf_error(_("handler or restart stack mismatch in old restart"));

    /**** need more here to keep recursive errors in browser? */
    rho = cptr->workingEnvironment();
    PROTECT(klass = Rf_mkChar("error"));
    entry = mkHandlerEntry(klass, rho, nullptr, rho, nullptr, TRUE);
    push_handler(entry);
    UNPROTECT(1);
    PROTECT(name = Rf_mkString(cname));
    PROTECT(entry = Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(entry, 0, name);
    SET_VECTOR_ELT(entry, 1, R_MakeExternalPtr(cptr, nullptr, nullptr));
    Rf_setAttrib(entry, Symbols::ClassSymbol, Rf_mkString("restart"));
    push_restart(entry);
    UNPROTECT(2);
}

HIDDEN SEXP do_dfltWarn(/*const*/ Expression* call, const BuiltInFunction* op, RObject* message_, RObject* call_)
{
    const char *msg;
    SEXP ecall;

    if (TYPEOF(message_) != STRSXP || LENGTH(message_) != 1)
	Rf_error(_("bad error message"));
    msg = Rf_translateChar(STRING_ELT(message_, 0));
    ecall = call_;

    warningcall_dflt(ecall, "%s", msg);
    return nullptr;
}

NORET HIDDEN SEXP do_dfltStop(/*const*/ Expression* call, const BuiltInFunction* op, RObject* message_, RObject* call_)
{
    const char *msg;
    SEXP ecall;

    if (TYPEOF(message_) != STRSXP || LENGTH(message_) != 1)
	Rf_error(_("bad error message"));
    msg = Rf_translateChar(STRING_ELT(message_, 0));
    ecall = call_;

    errorcall_dflt(ecall, "%s", msg);
}


/*
 * Restart Handling
 */

HIDDEN SEXP do_getRestart(/*const*/ Expression* call, const BuiltInFunction* op, RObject* i_)
{
    int i;
    SEXP list;
    i = Rf_asInteger(i_);
    for (list = R_RestartStack;
	 list != nullptr && i > 1;
	 list = CDR(list), i--);
    if (list != nullptr)
	return CAR(list);
    else if (i == 1) {
	/**** need to pre-allocate */
        GCStackRoot<> name(Rf_mkString("abort"));
	GCStackRoot<> entry(Rf_allocVector(VECSXP, 2));
	SET_VECTOR_ELT(entry, 0, name);
	SET_VECTOR_ELT(entry, 1, nullptr);
	Rf_setAttrib(entry, Symbols::ClassSymbol, Rf_mkString("restart"));
	return entry;
    }
    else return nullptr;
}

/* very minimal error checking --just enough to avoid a segfault */
namespace {
    inline void CHECK_RESTART(SEXP r)
    {
	if (TYPEOF(r) != VECSXP || LENGTH(r) < 2)
	    Rf_error(_("bad restart"));
    }
}

HIDDEN SEXP do_addRestart(/*const*/ Expression* call, const BuiltInFunction* op, RObject* restart)
{
    CHECK_RESTART(restart);
    push_restart(restart);
    return nullptr;
}

#define RESTART_EXIT(r) VECTOR_ELT(r, 1)

[[noreturn]] static void invokeRestart(SEXP r, SEXP arglist)
{
    SEXP exit = RESTART_EXIT(r);

    if (exit == nullptr) {
	R_RestartStack = nullptr;
	jump_to_toplevel();
    }
    else {
	for (; R_RestartStack != nullptr; pop_restart())
	    if (exit == RESTART_EXIT(CAR(R_RestartStack))) {
		pop_restart();
		if (TYPEOF(exit) == EXTPTRSXP)
		    throw CommandTerminated();
		else {
		    Environment* envir = SEXP_downcast<Environment*>(exit);
		    if (!envir->canReturn())
			Rf_error(_("no function to return from, jumping to top level"));
		    throw ReturnException(envir, arglist);
		}
	    }
	Rf_error(_("restart not on stack"));
    }
}

NORET HIDDEN SEXP do_invokeRestart(/*const*/ Expression* call, const BuiltInFunction* op, RObject* r_, RObject* args_)
{
    CHECK_RESTART(r_);
    invokeRestart(r_, args_);
}

HIDDEN SEXP do_seterrmessage(/*const*/ Expression* call, const BuiltInFunction* op, RObject* message_)
{
    SEXP msg;

    msg = message_;
    if(!Rf_isString(msg) || LENGTH(msg) != 1)
	Rf_error(_("error message must be a character string"));
    R_SetErrmessage(R_CHAR(STRING_ELT(msg, 0)));
    return nullptr;
}

HIDDEN SEXP
do_printDeferredWarnings(/*const*/ Expression* call, const BuiltInFunction* op)
{
    R_PrintDeferredWarnings();
    return nullptr;
}

/* These functions are to be used in error messages, and available for others to use in the API
   GetCurrentSrcref returns the first non-NULL srcref after skipping skip of them.  If it
   doesn't find one it returns NULL. */

SEXP
R_GetCurrentSrcref(int skip)
{
    FunctionContext* c = FunctionContext::innermost();
    SEXP srcref = R_Srcref;
    if (skip < 0) { /* to count up from the bottom, we need to count them all first */
	while (c) {
	    if (srcref && srcref != nullptr)
		skip++;
	    srcref = c->sourceLocation();
	    c = FunctionContext::innermost(c->nextOut());
	};
	if (skip < 0) return nullptr; /* not enough there */
	c = FunctionContext::innermost();
	srcref = R_Srcref;
    }
    while (c && (skip || !srcref)) {
    	if (srcref) 
	    skip--;
	srcref = c->sourceLocation();
	c = FunctionContext::innermost(c->nextOut());
    }
    if (skip || !srcref)
	srcref = nullptr;
    return srcref;
}

/* Return the filename corresponding to a srcref, or "" if none is found */

SEXP
R_GetSrcFilename(SEXP srcref)
{
    SEXP srcfile = Rf_getAttrib(srcref, Symbols::SrcfileSymbol);
    if (TYPEOF(srcfile) != ENVSXP)
	return Rf_ScalarString(Rf_mkChar(""));
    srcfile = Rf_findVar(Rf_install("filename"), srcfile);
    if (TYPEOF(srcfile) != STRSXP)
	return Rf_ScalarString(Rf_mkChar(""));
    return srcfile;
}


/*
 * C level tryCatch support
 */

/* There are two functions:

       R_TryCatchError    handles error conditions;

       R_TryCatch         can handle any condition type and allows a
                          finalize action.
*/

SEXP R_tryCatchError(SEXP (*body)(void *), void *bdata,
		     SEXP (*handler)(SEXP, void *), void *hdata)
{
    SEXP val;
    SEXP cond = Rf_mkString("error");

    PROTECT(cond);
    val = R_tryCatch(body, bdata, cond, handler, hdata, nullptr, nullptr);
    UNPROTECT(1);
    return val;
}

/* This implementation uses R's tryCatch via calls from C to R to
   invoke R's tryCatch, and then back to C to invoke the C
   body/handler functions via a .Internal helper. This makes the
   implementation fairly simple but not fast. If performance becomes
   an issue we can look into a pure C implementation. LT */

 struct tryCatchData_t {
    SEXP (*body)(void *);
    void *bdata;
    SEXP (*handler)(SEXP, void *);
    void *hdata;
    void (*finally)(void *);
    void *fdata;
    int suspended;
};

static SEXP default_tryCatch_handler(SEXP cond, void *data)
{
    return nullptr;
}

static void default_tryCatch_finally(void *data) { }

static SEXP trycatch_callback = nullptr;
static const char* trycatch_callback_source =
    "function(code, conds, fin) {\n"
    "    handler <- function(cond)\n"
    "        if (Rf_inherits(cond, conds))\n"
    "            .Internal(C_tryCatchHelper(code, 1L, cond))\n"
    "        else\n"
    "            signalCondition(cond)\n"
    "    if (fin)\n"
    "        tryCatch(.Internal(C_tryCatchHelper(code, 0L)),\n"
    "                 condition = handler,\n"
    "                 finally = .Internal(C_tryCatchHelper(code, 2L)))\n"
    "    else\n"
    "        tryCatch(.Internal(C_tryCatchHelper(code, 0L)),\n"
    "                 condition = handler)\n"
    "}";

SEXP R_tryCatch(SEXP (*body)(void *), void *bdata,
		SEXP conds,
		SEXP (*handler)(SEXP, void *), void *hdata,
		void (*finally)(void *), void *fdata)
{
    if (body == nullptr) Rf_error("must supply a body function");

    if (trycatch_callback == nullptr) {
	trycatch_callback = R_ParseEvalString(trycatch_callback_source,
					      R_BaseNamespace);
	R_PreserveObject(trycatch_callback);
    }

    tryCatchData_t tcd = {
	.body = body,
	.bdata = bdata,
	.handler = handler != nullptr ? handler : default_tryCatch_handler,
	.hdata = hdata,
	.finally = finally != nullptr ? finally : default_tryCatch_finally,
	.fdata = fdata,
	.suspended = R_interrupts_suspended
    };

    /* Interrupts are suspended while in the infrastructure R code and
       enabled, if the were on entry to R_TryCatch, while calling the
       body function in do_tryCatchHelper */

    R_interrupts_suspended = TRUE;

    if (conds == nullptr) conds = Rf_allocVector(STRSXP, 0);
    PROTECT(conds);
    SEXP fin = finally != nullptr ? R_TrueValue : R_FalseValue;
    SEXP tcdptr = R_MakeExternalPtr(&tcd, nullptr, nullptr);
    SEXP expr = Rf_lang4(trycatch_callback, tcdptr, conds, fin);
    PROTECT(expr);
    SEXP val = Rf_eval(expr, R_GlobalEnv);
    UNPROTECT(2); /* conds, expr */
    R_interrupts_suspended = Rboolean(tcd.suspended);
    return val;
}

SEXP do_tryCatchHelper(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP eptr = CAR(args);
    SEXP sw = CADR(args);
    SEXP cond = CADDR(args);

    if (TYPEOF(eptr) != EXTPTRSXP)
	Rf_error("not an external pointer");

    tryCatchData_t *ptcd = (tryCatchData_t*) R_ExternalPtrAddr(CAR(args));

    switch (Rf_asInteger(sw)) {
    case 0:
	if (ptcd->suspended)
	    /* Interrupts were suspended for the call to R_TryCatch,
	       so leave them that way */
	    return ptcd->body(ptcd->bdata);
	else {
	    /* Interrupts were not suspended for the call to
	       R_TryCatch, but were suspended for the call through
	       R. So enable them for the body and suspend again on the
	       way out. */
	    R_interrupts_suspended = FALSE;
	    SEXP val = ptcd->body(ptcd->bdata);
	    R_interrupts_suspended = TRUE;
	    return val;
	}
    case 1:
	if (ptcd->handler != nullptr)
	    return ptcd->handler(cond, ptcd->hdata);
	else return nullptr;
    case 2:
	if (ptcd->finally != nullptr)
	    ptcd->finally(ptcd->fdata);
	return nullptr;
    default: return nullptr; /* should not happen */
    }
}
