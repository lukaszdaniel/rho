/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2017   The R Core Team
 *  Copyright (C) 2002-2005  The R Foundation
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

/** @file main.cpp
 *
 * The main program.
 */

// For debugging:
#include <iostream>

#define R_NO_REMAP

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <math.h> /* avoid redefinition of extern in Defn.h */
#include <float.h>
#include <ctype.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>

#define __MAIN__
#include <Defn.h>
#include <Localization.h>
#include <Internal.h>
#include <Rinterface.h>
#include <IOStuff.h>
#include <Fileio.h>
#include <Parse.h>
#include <Rembedded.h>
#include <Startup.h>
#include "basedecl.h"

#include <locale.h>

#include "rho/ArgMatcher.hpp"
#include "rho/Browser.hpp"
#include "rho/ClosureContext.hpp"
#include "rho/CommandTerminated.hpp"
#include "rho/ListVector.hpp"
#include "rho/ProtectStack.hpp"
#include "rho/ProvenanceTracker.hpp"
#include "rho/ReturnException.hpp"
#include "rho/GCStackFrameBoundary.hpp"

using namespace rho;

#ifdef ENABLE_NLS
void attribute_hidden nl_Rdummy(void)
{
    /* force this in as packages use it */
    dgettext("R", "dummy - do not translate");
}
#endif


/* The 'real' main() program is in Rmain.cpp on Unix-alikes, and
   src/gnuwin/front-ends/graphappmain.c on Windows, unless of course
   R is embedded */

// Global variables: The story so far.  This is how things are done in CR:

/* Global Variables:  For convenience, all interpeter global symbols
 * ================   are declared in Defn.h as extern -- and defined here.
 *
 * NOTE: This is done by using some preprocessor trickery.  If __MAIN__
 * is defined as above, there is a sneaky
 *     #define extern
 * so that the same code produces both declarations and definitions.
 *
 * This does not include user interface symbols which are included
 * in separate platform dependent modules.
 */

// Now read on: In rho the preprocessor trickery referred to above is
// not used.  Gradually global variables will be replaced by variables
// with class or namespace scope, and their definitions migrated to
// the appropriate class-related source file, but those that have not
// yet been migrated are defined below.

// Data declared LibExtern in R_ext/Arith.h :

LibExport double R_NaN;		/* IEEE NaN */
LibExport double R_PosInf;	/* IEEE Inf */
LibExport double R_NegInf;	/* IEEE -Inf */
LibExport double R_NaReal;	/* NA_REAL: IEEE */
LibExport int	 R_NaInt;	/* NA_INTEGER:= INT_MIN currently */

// Data declared LibExtern in Rinternals.h :

LibExport SEXP	R_NamespaceRegistry;/* Registry for registered name spaces */

// Data declared LibExtern in Defn.h :

LibExport char *R_Home;		    /* Root of the R tree */
LibExport int	R_Is_Running;	    /* for Windows memory manager */
LibExport Rboolean R_Interactive = TRUE;  /* TRUE during interactive use*/
LibExport char *R_TempDir = nullptr;   /* Name of per-session dir */
LibExport char *R_HistoryFile;	    /* Name of the history file */
LibExport int	R_HistorySize;	    /* Size of the history file */
LibExport int	R_RestoreHistory;   /* restore the history file? */
LibExport Rboolean utf8locale = FALSE;  /* is this a UTF-8 locale? */
LibExport Rboolean mbcslocale = FALSE;  /* is this a MBCS locale? */
LibExport unsigned int localeCP = 1252; /* the locale's codepage */
LibExport int R_num_math_threads = 1;
LibExport int R_max_num_math_threads = 1;
LibExport SEXP R_MethodsNamespace;
LibExport AccuracyInfo R_AccuracyInfo;
LibExport SEXP R_TrueValue = NULL;
LibExport SEXP R_FalseValue = NULL;
LibExport SEXP R_LogicalNAValue = NULL;
LibExport Rboolean R_PCRE_use_JIT = TRUE;
LibExport int R_PCRE_study = 10;
LibExport int R_PCRE_limit_recursion;
// Data declared extern in Defn.h :

int	gc_inhibit_torture = 1;
uintptr_t R_CStackLimit	= uintptr_t(-1);	/* C stack limit */
uintptr_t R_CStackStart	= uintptr_t(-1);	/* Initial stack address */
Rboolean  R_Slave	= FALSE;	/* Run as a slave process */
FILE*	R_Consolefile	= nullptr;	/* Console output file */
FILE*	R_Outputfile	= nullptr;	/* Output file */
int	R_DirtyImage	= 1;	/* Current image dirty */
const char	*R_GUIType	= "unknown";
Rboolean R_isForkedChild = FALSE;
double cpuLimit = -1.0;
double cpuLimit2 = -1.0;
double cpuLimitValue = -1.0;
double elapsedLimit = -1.0;
double elapsedLimit2 = -1.0;
double elapsedLimitValue = -1.0;

// Data declared extern0 in Defn.h :

attribute_hidden R_size_t R_VSize  = R_VSIZE;/* Initial size of the heap */

attribute_hidden int    R_EvalDepth     = 0; 	/* Evaluation recursion depth */
attribute_hidden int	R_BrowseLines	= 0;	/* lines/per call in browser */
attribute_hidden Rboolean R_KeepSource	= FALSE;	/* options(keep.source) */
attribute_hidden Rboolean R_CBoundsCheck = FALSE;	/* options(CBoundsCheck) */
attribute_hidden size_t	R_WarnLength	= 1000;	/* Error/warning max length */
attribute_hidden MATPROD_TYPE R_Matprod = MATPROD_DEFAULT; /* options(matprod) */
attribute_hidden int    R_nwarnings     = 50;
attribute_hidden int	R_CStackDir	= 1;	/* C stack direction */
attribute_hidden Rboolean R_WarnEscapes  = TRUE;   /* Warn on unrecognized escapes */
attribute_hidden Rboolean R_Quiet	= FALSE;	/* Be as quiet as possible */
attribute_hidden Rboolean R_Verbose	= FALSE;	/* Be verbose */
attribute_hidden int	R_ErrorCon	= 2;	/* Error connection */
attribute_hidden char   *Sys_TempDir	= nullptr;	/* Name of per-session dir
						   if set by R itself */
attribute_hidden char	R_StdinEnc[31]  = "";	/* Encoding assumed for stdin */
int R_ParseError	= 0; /* Line where parse error occurred */
attribute_hidden int	R_ParseErrorCol;    /* Column of start of token where parse error occurred */
attribute_hidden SEXP	R_ParseErrorFile;   /* Source file where parse error was seen */
char R_ParseErrorMsg[PARSE_ERROR_SIZE] = "";
char R_ParseContext[PARSE_CONTEXT_SIZE] = "";
int R_ParseContextLast = 0; /* last character in context buffer */
int R_ParseContextLine; /* Line in file of the above */
attribute_hidden int	R_CollectWarnings = 0;	/* the number of warnings */
GCRoot<ListVector>	R_Warnings;	    /* the warnings and their calls */
attribute_hidden int	R_ShowErrorMessages = 1;     /* show error messages? */
attribute_hidden Rboolean R_warn_partial_match_dollar = FALSE;
attribute_hidden Rboolean R_warn_partial_match_attr = FALSE;
attribute_hidden Rboolean R_ShowWarnCalls = FALSE;
attribute_hidden Rboolean R_ShowErrorCalls = FALSE;
attribute_hidden int R_NShowCalls = 50;
attribute_hidden   Rboolean latin1locale = FALSE; /* is this a Latin-1 locale? */
const char* OutDec = ".";  /* decimal point used for output */
attribute_hidden Rboolean R_DisableNLinBrowser = FALSE;
attribute_hidden char R_BrowserLastCommand = 'n';
attribute_hidden int R_check_constants = 0;
attribute_hidden int R_dec_min_exponent		= -308;
unsigned int max_contour_segments = 25000;
Rboolean known_to_be_latin1 = FALSE;
Rboolean known_to_be_utf8 = FALSE;

// Data declared LibExtern in Rembedded.h :

//LibExport int R_DirtyImage;
//LibExport char *R_TempDir;

#ifdef Win32   
LibExport int UserBreak;
#endif



void Rf_callToplevelHandlers(SEXP expr, SEXP value, Rboolean succeeded,
			     Rboolean visible);

static int ParseBrowser(SEXP, SEXP);


	/* Read-Eval-Print Loop [ =: REPL = repl ] with input from a file */

static RObject* R_ReplFile_impl(FILE *fp, SEXP rho)
{
    ParseStatus status;
    int count=0;
    std::size_t savestack;

    R_InitSrcRefState();
    savestack = ProtectStack::size();
    for(;;) {
	ProtectStack::restoreSize(savestack);
	R_CurrentExpr = R_Parse1File(fp, 1, &status);
	switch (status) {
	case PARSE_NULL:
	    break;
	case PARSE_OK:
	    R_Visible = FALSE;
	    resetTimeLimits();
	    count++;
	    PROTECT(R_CurrentExpr);
	    R_CurrentExpr = Rf_eval(R_CurrentExpr, rho);
	    SET_SYMVALUE(R_LastvalueSymbol, R_CurrentExpr);
	    UNPROTECT(1);
	    if (R_Visible)
		Rf_PrintValueEnv(R_CurrentExpr, rho);
	    if( R_CollectWarnings )
		Rf_PrintWarnings(nullptr);
	    break;
	case PARSE_ERROR:
	    R_FinalizeSrcRefState();
	    parseError(R_NilValue, R_ParseError);
	    break;
	case PARSE_EOF:
	    R_FinalizeSrcRefState();
	    return nullptr;
	    break;
	case PARSE_INCOMPLETE:
	    /* can't happen: just here to quieten -Wall */
	    break;
	}
    }
    return nullptr;
}

static void R_ReplFile(FILE *fp, SEXP rho) {
    GCStackFrameBoundary::withStackFrameBoundary(
	std::bind(R_ReplFile_impl, fp, rho));
}

/* Read-Eval-Print loop with interactive input */
static int prompt_type;
static char BrowsePrompt[20];

static const char *R_PromptString(int browselevel, int type)
{
    if (R_Slave) {
	BrowsePrompt[0] = '\0';
	return BrowsePrompt;
    }
    else {
	if(type == 1) {
	    if(browselevel) {
		snprintf(BrowsePrompt, 20, "Browse[%d]> ", browselevel);
		return BrowsePrompt;
	    }
	    return R_CHAR(STRING_ELT(Rf_GetOption1(Rf_install("prompt")), 0));
	}
	else {
	    return R_CHAR(STRING_ELT(Rf_GetOption1(Rf_install("continue")), 0));
	}
    }
}

/*
  This is a reorganization of the REPL (Read-Eval-Print Loop) to separate
  the loop from the actions of the body. The motivation is to make the iteration
  code (Rf_ReplIteration) available as a separately callable routine
  to avoid cutting and pasting it when one wants a single iteration
  of the loop. This is needed as we allow different implementations
  of event loops. Currently (summer 2002), we have a package in
  preparation that uses Rf_ReplIteration within either the
  Tcl or Gtk event loop and allows either (or both) loops to
  be used as a replacement for R's loop and take over the event
  handling for the R process.

  The modifications here are intended to leave the semantics of the REPL
  unchanged, just separate into routines. So the variables that maintain
  the state across iterations of the loop are organized into a structure
  and passed to Rf_ReplIteration() from Rf_ReplConsole().
*/


/*
  (local) Structure for maintaining and exchanging the state between
  Rf_ReplConsole and its worker routine Rf_ReplIteration which is the
  implementation of the body of the REPL.

  In the future, we may need to make this accessible to packages
  and so put it into one of the public R header files.
 */
struct R_ReplState {
  ParseStatus    status;
  int            prompt_type;
  int            browselevel;
  unsigned char  buf[CONSOLE_BUFFER_SIZE+1];
  unsigned char *bufp;
};


/**
  This is the body of the REPL.
  It attempts to parse the first line or expression of its input,
  and optionally request input from the user if none is available.
  If the input can be parsed correctly,
     i) the resulting expression is evaluated,
    ii) the result assigned to .Last.Value,
   iii) top-level task handlers are invoked.

 If the input cannot be parsed, i.e. there is a syntax error,
 it is incomplete, or we encounter an end-of-file, then we
 change the prompt accordingly.

 The "cursor" for the input buffer is moved to the next starting
 point, i.e. the end of the first line or after the first ;.
 */
int
Rf_ReplIteration(SEXP rho, unsigned int savestack, R_ReplState *state)
{
    int c, browsevalue;
    SEXP value, thisExpr;
    Rboolean wasDisplayed = FALSE;
    int browselevel = int(Browser::numberActive());

    if(!*state->bufp) {
	    R_Busy(0);
	    if (R_ReadConsole(R_PromptString(browselevel, state->prompt_type),
			      state->buf, CONSOLE_BUFFER_SIZE, 1) == 0)
		return(-1);
	    state->bufp = state->buf;
    }
#ifdef SHELL_ESCAPE /* not default */
    if (*state->bufp == '!') {
	    R_system(&(state->buf[1]));
	    state->buf[0] = '\0';
	    return(0);
    }
#endif /* SHELL_ESCAPE */
    while((c = *state->bufp++)) {
	    R_IoBufferPutc(c, &R_ConsoleIob);
	    if(c == ';' || c == '\n') break;
    }

    ProtectStack::restoreSize(savestack);
    R_CurrentExpr = R_Parse1Buffer(&R_ConsoleIob, 0, &state->status);

    switch(state->status) {

    case PARSE_NULL:

	/* The intention here is to break on CR but not on other
	   null statements: see PR#9063 */
	if (browselevel && !R_DisableNLinBrowser
	    && streql(reinterpret_cast<char *>(state->buf), "\n")) return -1;
	R_IoBufferWriteReset(&R_ConsoleIob);
	state->prompt_type = 1;
	return 1;

    case PARSE_OK:
	{
	    R_IoBufferReadReset(&R_ConsoleIob);
	    R_CurrentExpr = R_Parse1Buffer(&R_ConsoleIob, 1, &state->status);
#ifdef PROVENANCE_TRACKING
	    ProvenanceTracker::CommandScope scope(R_CurrentExpr);
#endif
	    if (browselevel) {
		browsevalue = ParseBrowser(R_CurrentExpr, rho);
		if(browsevalue == 1) return -1;
		if(browsevalue == 2) {
		    R_IoBufferWriteReset(&R_ConsoleIob);
		    return 0;
		}
	    }
	    /* PR#15770 We don't want to step into expressions entered at the debug prompt.
	       The 'S' will be changed back to 's' after the next eval. */
	    if (R_BrowserLastCommand == 's') R_BrowserLastCommand = 'S';  
	    R_Visible = FALSE;
	    R_EvalDepth = 0;
	    resetTimeLimits();
	    PROTECT(thisExpr = R_CurrentExpr);
	    R_Busy(1);
	    value = Rf_eval(thisExpr, rho);
	    SET_SYMVALUE(R_LastvalueSymbol, value);
	    wasDisplayed = R_Visible;
	    if (R_Visible)
		Rf_PrintValueEnv(value, rho);
	    if (R_CollectWarnings)
		Rf_PrintWarnings(nullptr);
	    Rf_callToplevelHandlers(thisExpr, value, TRUE, wasDisplayed);
	    R_CurrentExpr = value; /* Necessary? Doubt it. */
	    UNPROTECT(1);
	    if (R_BrowserLastCommand == 'S') R_BrowserLastCommand = 's';  
	    R_IoBufferWriteReset(&R_ConsoleIob);
	    state->prompt_type = 1;
	    return(1);
	}

    case PARSE_ERROR:

	state->prompt_type = 1;
	parseError(R_NilValue, 0);
	R_IoBufferWriteReset(&R_ConsoleIob);
	return(1);

    case PARSE_INCOMPLETE:

	R_IoBufferReadReset(&R_ConsoleIob);
	state->prompt_type = 2;
	return(2);

    case PARSE_EOF:

	return(-1);
	break;
    }

    return(0);
}

static RObject* R_ReplConsole_impl(SEXP rho, int savestack)
{
    int status;
    R_ReplState state = { PARSE_NULL, 1, 0, "", nullptr};

    R_IoBufferWriteReset(&R_ConsoleIob);
    state.buf[0] = '\0';
    state.buf[CONSOLE_BUFFER_SIZE] = '\0';
    /* stopgap measure if line > CONSOLE_BUFFER_SIZE chars */
    state.bufp = state.buf;
    if(R_Verbose)
	REprintf(" >R_ReplConsole(): before \"for(;;)\" {main.cpp}\n");
    for(;;) {
	status = Rf_ReplIteration(rho, savestack, &state);
	if(status < 0) {
	  if (state.status == PARSE_INCOMPLETE)
	    Rf_error(_("unexpected end of input"));
	  return nullptr;
	}
    }
}

static void R_ReplConsole(SEXP rho, int savestack)
{
    GCStackFrameBoundary::withStackFrameBoundary(
	std::bind(R_ReplConsole_impl, rho, savestack));
}

/* A simple customized print of the traceback */
static void printTraceback(SEXP trace) {
    int line = 1;
    if(trace != R_NilValue) {
	PROTECT(trace);
	REprintf("\nTraceback:\n");
	for(SEXP p = trace; p != R_NilValue; p = CDR(p), line++) {
	    SEXP q = CAR(p); /* a character vector */
	    REprintf("%2d: ", line);
	    for(int i = 0; i < LENGTH(q); i++)
		REprintf("%s", R_CHAR(STRING_ELT(q, i)));
	    REprintf("\n");
	}
	UNPROTECT(1);
    }
}

static unsigned char DLLbuf[CONSOLE_BUFFER_SIZE+1], *DLLbufp;

static void check_session_exit()
{
    if (! R_Interactive) {
        /* If an error occurs while we are exiting due to an error,
           then call R_Suicide()
        */
	static bool exiting = false;
	if (exiting)
	    R_Suicide(_("error during cleanup\n"));
        if (Rf_GetOption1(Rf_install("error")) != R_NilValue)
            return;

        exiting = true;
	printTraceback(SYMVALUE(Rf_install(".Traceback")));;
        REprintf(_("\nExecution halted\n"));

        R_CleanUp(SA_NOSAVE, 1, 0); /* quit, no save, no .Last, status=1 */
    }
}

void R_ReplDLLinit(void)
{
    while(true) {
        try {
            R_IoBufferWriteReset(&R_ConsoleIob);
            prompt_type = 1;
            DLLbuf[0] = DLLbuf[CONSOLE_BUFFER_SIZE] = '\0';
            DLLbufp = DLLbuf;
            break;
        } catch (CommandTerminated) {
            check_session_exit();
            continue;
        }
    }
}

/* FIXME: this should be re-written to use Rf_ReplIteration
   since it gets out of sync with it over time */
int R_ReplDLLdo1(void)
{
    int c;
    ParseStatus status;
    SEXP rho = R_GlobalEnv, lastExpr;
    Rboolean wasDisplayed = FALSE;

    if(!*DLLbufp) {
	R_Busy(0);
	if (R_ReadConsole(R_PromptString(0, prompt_type), DLLbuf,
			  CONSOLE_BUFFER_SIZE, 1) == 0)
	    return -1;
	DLLbufp = DLLbuf;
    }
    while((c = *DLLbufp++)) {
	R_IoBufferPutc(c, &R_ConsoleIob);
	if(c == ';' || c == '\n') break;
    }
    ProtectStack::restoreSize(0);
    R_CurrentExpr = R_Parse1Buffer(&R_ConsoleIob, 0, &status);

    switch(status) {
    case PARSE_NULL:
	R_IoBufferWriteReset(&R_ConsoleIob);
	prompt_type = 1;
	break;
    case PARSE_OK:
	R_IoBufferReadReset(&R_ConsoleIob);
	R_CurrentExpr = R_Parse1Buffer(&R_ConsoleIob, 1, &status);
	R_Visible = FALSE;
	resetTimeLimits();
	PROTECT(R_CurrentExpr);
	R_Busy(1);
	lastExpr = R_CurrentExpr;
	R_CurrentExpr = Rf_eval(R_CurrentExpr, rho);
	SET_SYMVALUE(R_LastvalueSymbol, R_CurrentExpr);
	wasDisplayed = R_Visible;
	if (R_Visible)
	    Rf_PrintValueEnv(R_CurrentExpr, rho);
	if (R_CollectWarnings)
	    Rf_PrintWarnings(nullptr);
	Rf_callToplevelHandlers(lastExpr, R_CurrentExpr, TRUE, wasDisplayed);
	UNPROTECT(1);
	R_IoBufferWriteReset(&R_ConsoleIob);
	R_Busy(0);
	prompt_type = 1;
	break;
    case PARSE_ERROR:
	parseError(R_NilValue, 0);
	R_IoBufferWriteReset(&R_ConsoleIob);
	prompt_type = 1;
	break;
    case PARSE_INCOMPLETE:
	R_IoBufferReadReset(&R_ConsoleIob);
	prompt_type = 2;
	break;
    case PARSE_EOF:
	return -1;
	break;
    }
    return prompt_type;
}


/* Main Loop: It is assumed that at this point that operating system */
/* specific tasks (dialog window creation etc) have been performed. */
/* We can now print a greeting, run the .First function and then enter */
/* the read-eval-print loop. */

static RETSIGTYPE handleInterrupt(int dummy)
{
    R_interrupts_pending = 1;
    signal(SIGINT, handleInterrupt);
}

/* this flag is set if R internal code is using send() and does not
   want to trigger an error on SIGPIPE (e.g., the httpd code).
   [It is safer and more portable than other methods of handling
   broken pipes on send().]
 */

#ifndef Win32
// controlled by the internal http server in the internet module
int R_ignore_SIGPIPE = 0;

static RETSIGTYPE handlePipe(int dummy)
{
    signal(SIGPIPE, handlePipe);
    Rf_warning("ignoring SIGPIPE signal");
}
#endif


#ifdef Win32
static int num_caught = 0;

static void win32_segv(int signum)
{
    /* NB: stack overflow is not an access violation on Win32 */
    printTraceback(R_GetTraceback(0));

    num_caught++;
    if(num_caught < 10) signal(signum, win32_segv);
    if(signum == SIGILL)
	Rf_error("caught access violation - continue with care");
    else
	Rf_error("caught access violation - continue with care");
}
#endif

#if defined(HAVE_SIGALTSTACK) && defined(HAVE_SIGACTION) && defined(HAVE_WORKING_SIGACTION) && defined(HAVE_SIGEMPTYSET)

/* NB: this really isn't safe, but suffices for experimentation for now.
   In due course just set a flag and do this after the return.  OTOH,
   if we do want to bail out with a core dump, need to do that here.

   2005-12-17 BDR */

static char ConsoleBuf[CONSOLE_BUFFER_SIZE];

static struct sigaction previous_handlers[NSIG];

static void sigactionSegv(int signum, siginfo_t *ip, void *context)
{
    const char *s;

    /* First check for stack overflow if we know the stack position.
       We assume anything within 16Mb beyond the stack end is a stack overflow.
     */
    if(signum == SIGSEGV && (ip != nullptr) &&
       intptr_t(R_CStackStart) != -1) {
	uintptr_t addr = uintptr_t( ip->si_addr);
	intptr_t diff = (R_CStackDir > 0) ? R_CStackStart - addr:
	    addr - R_CStackStart;
	uintptr_t upper = 0x1000000;  /* 16Mb */
	if(intptr_t(R_CStackLimit) != -1) upper += R_CStackLimit;
	if(diff > 0 && diff < int(upper)) {
	    REprintf(_("Error: segfault from C stack overflow\n"));
	    jump_to_toplevel();
	}
    }

    /* need to take off stack checking as stack base has changed */
    R_CStackLimit = uintptr_t(-1);

    /* Do not translate these messages */
    REprintf("\n *** caught %s ***\n",
	     signum == SIGILL ? "illegal operation" :
	     signum == SIGBUS ? "bus error" : "segfault");
    if(ip != nullptr) {
	if(signum == SIGILL) {

	    switch(ip->si_code) {
#ifdef ILL_ILLOPC
	    case ILL_ILLOPC:
		s = "illegal opcode";
		break;
#endif
#ifdef ILL_ILLOPN
	    case ILL_ILLOPN:
		s = "illegal operand";
		break;
#endif
#ifdef ILL_ILLADR
	    case ILL_ILLADR:
		s = "illegal addressing mode";
		break;
#endif
#ifdef ILL_ILLTRP
	    case ILL_ILLTRP:
		s = "illegal trap";
		break;
#endif
#ifdef ILL_COPROC
	    case ILL_COPROC:
		s = "coprocessor error";
		break;
#endif
	    default:
		s = "unknown";
		break;
	    }
	} else if(signum == SIGBUS)
	    switch(ip->si_code) {
#ifdef BUS_ADRALN
	    case BUS_ADRALN:
		s = "invalid alignment";
		break;
#endif
#ifdef BUS_ADRERR /* not on macOS, apparently */
	    case BUS_ADRERR:
		s = "non-existent physical address";
		break;
#endif
#ifdef BUS_OBJERR /* not on macOS, apparently */
	    case BUS_OBJERR:
		s = "object specific hardware error";
		break;
#endif
	    default:
		s = "unknown";
		break;
	    }
	else
	    switch(ip->si_code) {
#ifdef SEGV_MAPERR
	    case SEGV_MAPERR:
		s = "memory not mapped";
		break;
#endif
#ifdef SEGV_ACCERR
	    case SEGV_ACCERR:
		s = "invalid permissions";
		break;
#endif
	    default:
		s = "unknown";
		break;
	    }
	REprintf("address %p, cause '%s'\n", ip->si_addr, s);
    }
    printTraceback(R_GetTraceback(0));

    if(R_Interactive) {
	REprintf("\nPossible actions:\n1: %s\n2: %s\n3: %s\n4: %s\n",
		 "abort (with core dump, if enabled)",
		 "normal R exit",
		 "exit R without saving workspace",
		 "exit R saving workspace");
	while(1) {
	    REprintf("Selection: ");
	    if (fgets(ConsoleBuf, CONSOLE_BUFFER_SIZE, stdin)) {
		if(ConsoleBuf[0] == '1') break;
		if(ConsoleBuf[0] == '2') R_CleanUp(SA_DEFAULT, 0, 1);
		if(ConsoleBuf[0] == '3') R_CleanUp(SA_NOSAVE, 70, 0);
		if(ConsoleBuf[0] == '4') R_CleanUp(SA_SAVE, 71, 0);
	    }
	}
	REprintf("R is aborting now ...\n");
    }
    else // non-interactively :
	REprintf("An irrecoverable exception occurred. R is aborting now ...\n");
    R_CleanTempDir();

    /* Call the previous signal handler. */
    const struct sigaction& previous_handler = previous_handlers[signum];
    if (previous_handler.sa_flags & SA_SIGINFO) {
	(*previous_handler.sa_handler)(signum);
    } else {
	(*previous_handler.sa_sigaction)(signum, ip, context);
    }
}

#ifndef SIGSTKSZ
# define SIGSTKSZ 8192    /* just a guess */
#endif

#ifdef HAVE_STACK_T
static stack_t sigstk;
#else
static struct sigaltstack sigstk;
#endif
static void *signal_stack;

#define R_USAGE 100000 /* Just a guess */
static void init_signal_handlers(void)
{
    /* <FIXME> may need to reinstall this if we do recover. */
    struct sigaction sa;
    signal_stack = malloc(SIGSTKSZ + R_USAGE);
    if (signal_stack != nullptr) {
	sigstk.ss_sp = signal_stack;
	sigstk.ss_size = SIGSTKSZ + R_USAGE;
	sigstk.ss_flags = 0;
	if(sigaltstack(&sigstk, nullptr) < 0)
	    Rf_warning("failed to set alternate signal stack");
    } else
	Rf_warning("failed to allocate alternate signal stack");
    sa.sa_sigaction = sigactionSegv;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = SA_ONSTACK | SA_SIGINFO;
    sigaction(SIGSEGV, &sa, &previous_handlers[SIGSEGV]);
    sigaction(SIGILL, &sa, &previous_handlers[SIGILL]);
#ifdef SIGBUS
    sigaction(SIGBUS, &sa, &previous_handlers[SIGBUS]);
#endif

    signal(SIGINT,  handleInterrupt);
    signal(SIGUSR1, Rf_onsigusr1);
    signal(SIGUSR2, Rf_onsigusr2);
    signal(SIGPIPE, handlePipe);
}

#else /* not sigaltstack and sigaction and sigemptyset*/
static void init_signal_handlers(void)
{
    signal(SIGINT,  handleInterrupt);
    signal(SIGUSR1, Rf_onsigusr1);
    signal(SIGUSR2, Rf_onsigusr2);
#ifndef Win32
    signal(SIGPIPE, handlePipe);
#else
    signal(SIGSEGV, win32_segv);
    signal(SIGILL, win32_segv);
#endif
}
#endif


static void R_LoadProfile(FILE *fparg, SEXP env)
{
    FILE * volatile fp = fparg; /* is this needed? */
    if (fp != nullptr) {
	Evaluator evalr;
	try {
	    R_ReplFile(fp, env);
	}
	catch (CommandTerminated) {
          check_session_exit();
	}
	fclose(fp);
    }
}


int R_SignalHandlers = 1;  /* Exposed in R_interface.h */ // 2007/07/23 arr

const char* get_workspace_name();  /* from startup.cpp */

extern "C"
void attribute_hidden BindDomain(char *R_Home)
{
#ifdef ENABLE_NLS
    char localedir[PATH_MAX+20];
    setlocale(LC_MESSAGES,"");
    textdomain(PACKAGE);
    char *p = getenv("R_TRANSLATIONS");
    if (p) snprintf(localedir, PATH_MAX+20, "%s", p);
    else snprintf(localedir, PATH_MAX+20, "%s/library/translations", R_Home);
    bindtextdomain(PACKAGE, localedir); // PACKAGE = DOMAIN = "R"
    bindtextdomain("R-base", localedir);
# ifdef _WIN32
    bindtextdomain("RGui", localedir);
# endif
#endif
}

void setup_Rmainloop(void)
{
    volatile SEXP baseEnv;
    SEXP cmd;
    char deferred_warnings[11][250];
    volatile int ndeferred_warnings = 0;

#if 0 
    /* testing stack base and size detection */
    printf("stack limit %ld, start %lx dir %d \n",
	(unsigned long) R_CStackLimit,
        (unsigned long) R_CStackStart,
	R_CStackDir);
    uintptr_t firstb = R_CStackStart - R_CStackDir;
    printf("first accessible byte %lx\n", (unsigned long) firstb);
    if (R_CStackLimit != (uintptr_t)(-1)) {
        uintptr_t lastb = R_CStackStart - R_CStackDir * R_CStackLimit;
	printf("last accessible byte %lx\n", (unsigned long) lastb);
    }
    printf("accessing first byte...\n");
    volatile char dummy = *(char *)firstb;
    if (R_CStackLimit != (uintptr_t)(-1)) {
	printf("accessing all bytes...\n");
	/* have to access all bytes in order to map stack, e.g. on Linux */
	for(uintptr_t o = 0; o < R_CStackLimit; o++)
	    /* with exact bounds, o==-1 and o==R_CStackLimit will segfault */
	    dummy = *((char *)firstb - R_CStackDir * o);
    }
#endif

    /* In case this is a silly limit: 2^32 -3 has been seen and
     * casting to intptr_r relies on this being smaller than 2^31 on a
     * 32-bit platform. */
    if(R_CStackLimit > 100000000U)
	R_CStackLimit = (uintptr_t)-1;
    /* make sure we have enough head room to handle errors */
    if(R_CStackLimit != (uintptr_t)-1)
	R_CStackLimit = (uintptr_t)(0.95 * R_CStackLimit);

    Rf_InitConnections(); /* needed to get any output at all */

    /* Initialize the interpreter's internal structures. */

#ifdef HAVE_LOCALE_H
#ifdef Win32
    {
	char *p, Rlocale[1000]; /* Windows' locales can be very long */
	p = getenv("LC_ALL");
	strncpy(Rlocale, p ? p : "", 1000);
	Rlocale[1000 - 1] = '\0';
	if(!(p = getenv("LC_CTYPE"))) p = Rlocale;
	/* We'd like to use warning, but need to defer.
	   Also cannot translate. */
	if(!setlocale(LC_CTYPE, p))
	    snprintf(deferred_warnings[ndeferred_warnings++], 250,
		     "Setting LC_CTYPE=%s failed\n", p);
	if((p = getenv("LC_COLLATE"))) {
	    if(!setlocale(LC_COLLATE, p))
		snprintf(deferred_warnings[ndeferred_warnings++], 250,
			 "Setting LC_COLLATE=%s failed\n", p);
	} else setlocale(LC_COLLATE, Rlocale);
	if((p = getenv("LC_TIME"))) {
	    if(!setlocale(LC_TIME, p))
		snprintf(deferred_warnings[ndeferred_warnings++], 250,
			 "Setting LC_TIME=%s failed\n", p);
	} else setlocale(LC_TIME, Rlocale);
	if((p = getenv("LC_MONETARY"))) {
	    if(!setlocale(LC_MONETARY, p))
		snprintf(deferred_warnings[ndeferred_warnings++], 250,
			 "Setting LC_MONETARY=%s failed\n", p);
	} else setlocale(LC_MONETARY, Rlocale);
	/* Windows does not have LC_MESSAGES */

	/* We set R_ARCH here: Unix does it in the shell front-end */
	char Rarch[30];
	strcpy(Rarch, "R_ARCH=/");
	strcat(Rarch, R_ARCH);
	putenv(Rarch);
    }
#else /* not Win32 */
    if(!setlocale(LC_CTYPE, ""))
	snprintf(deferred_warnings[ndeferred_warnings++], 250,
		 "Setting LC_CTYPE failed, using \"C\"\n");
    if(!setlocale(LC_COLLATE, ""))
	snprintf(deferred_warnings[ndeferred_warnings++], 250,
		 "Setting LC_COLLATE failed, using \"C\"\n");
    if(!setlocale(LC_TIME, ""))
	snprintf(deferred_warnings[ndeferred_warnings++], 250,
		 "Setting LC_TIME failed, using \"C\"\n");
#ifdef ENABLE_NLS
    if(!setlocale(LC_MESSAGES, ""))
	snprintf(deferred_warnings[ndeferred_warnings++], 250,
		 "Setting LC_MESSAGES failed, using \"C\"\n");
#endif
    /* NB: we do not set LC_NUMERIC */
#ifdef LC_MONETARY
    if(!setlocale(LC_MONETARY, ""))
	snprintf(deferred_warnings[ndeferred_warnings++], 250,
		 "Setting LC_MONETARY failed, using \"C\"\n");
#endif
#ifdef LC_PAPER
    if(!setlocale(LC_PAPER, ""))
	snprintf(deferred_warnings[ndeferred_warnings++], 250,
		 "Setting LC_PAPER failed, using \"C\"\n");
#endif
#ifdef LC_MEASUREMENT
    if(!setlocale(LC_MEASUREMENT, ""))
	snprintf(deferred_warnings[ndeferred_warnings++], 250,
		 "Setting LC_MEASUREMENT failed, using \"C\"\n");
#endif
#endif /* not Win32 */
#endif

    /* make sure srand is called before R_tmpnam, PR#14381 */
    srand(TimeToSeed());

    Rf_InitArithmetic();
    InitParser();
    Rf_InitTempDir(); /* must be before InitEd */
    Rf_InitMemory();
    Rf_InitNames();
    Rf_InitGlobalEnv();
    InitDynload();
    Rf_InitOptions();
    Rf_InitEd();
    Rf_InitGraphics();
    Rf_InitTypeTables(); /* must be before InitS3DefaultTypes */
    Rf_InitS3DefaultTypes();
    Rf_PrintDefaults();

    R_Is_Running = 1;
    R_check_locale();

    R_Warnings = R_NilValue;

    /* This is the same as R_BaseEnv, but this marks the environment
       of functions as the namespace and not the package. */
    baseEnv = R_BaseNamespace;

    /* Set up some global variables */
    Init_R_Variables(baseEnv);

    /* On initial entry we open the base language package and begin by
       running the repl on it.
       If there is an error we pass on to the repl.
       Perhaps it makes more sense to quit gracefully?
    */

#ifdef RMIN_ONLY
    /* This is intended to support a minimal build for experimentation. */
    if (R_SignalHandlers) init_signal_handlers();
#else
    FILE *fp = R_OpenLibraryFile("base");
    if (fp == NULL)
	R_Suicide(_("unable to open the base package\n"));

    try {
	if (R_SignalHandlers) init_signal_handlers();
	R_ReplFile(fp, baseEnv);
    }
    catch (CommandTerminated) {
        check_session_exit();
    }
    fclose(fp);
#endif

    /* This is where we source the system-wide, the site's and the
       user's profile (in that order).  If there is an error, we
       drop through to further processing.
    */
    R_IoBufferInit(&R_ConsoleIob);
    R_LoadProfile(R_OpenSysInitFile(), baseEnv);
    /* These are the same bindings, so only lock them once */
    R_LockEnvironment(R_BaseNamespace, TRUE);
#ifdef NOTYET
    /* methods package needs to trample here */
    R_LockEnvironment(R_BaseEnv, TRUE);
#endif
    R_unLockBinding(R_LastvalueSymbol, R_BaseEnv);  // rho addition
    /* At least temporarily unlock some bindings used in graphics */
    R_unLockBinding(R_DeviceSymbol, R_BaseEnv);
    R_unLockBinding(R_DevicesSymbol, R_BaseEnv);
    R_unLockBinding(Rf_install(".Library.site"), R_BaseEnv);

    /* require(methods) if it is in the default packages */
    try {
	PROTECT(cmd = Rf_install(".OptRequireMethods"));
	R_CurrentExpr = Rf_findVar(cmd, R_GlobalEnv);
	if (R_CurrentExpr != R_UnboundValue &&
	    TYPEOF(R_CurrentExpr) == CLOSXP) {
		PROTECT(R_CurrentExpr = Rf_lang1(cmd));
		R_CurrentExpr = Rf_eval(R_CurrentExpr, R_GlobalEnv);
		UNPROTECT(1);
	}
	UNPROTECT(1);
    }
    catch (CommandTerminated) {
      check_session_exit();
    }

    if (streql(R_GUIType, "Tk")) {
	char buf[PATH_MAX];

	snprintf(buf, PATH_MAX, "%s/library/tcltk/exec/Tk-frontend.R", R_Home);
	R_LoadProfile(R_fopen(buf, "r"), R_GlobalEnv);
    }

    /* Print a platform and version dependent greeting and a pointer to
     * the copyleft.
     */
    if(!R_Quiet) Rf_PrintGreeting();

    R_LoadProfile(R_OpenSiteFile(), baseEnv);
    R_LockBinding(Rf_install(".Library.site"), R_BaseEnv);
    R_LoadProfile(R_OpenInitFile(), R_GlobalEnv);

    /* This is where we try to load a user's saved data.
       The right thing to do here is very platform dependent.
       E.g. under Unix we look in a special hidden file and on the Mac
       we look in any documents which might have been double clicked on
       or dropped on the application.
    */
    try {
	R_InitialData();
    }
    catch (CommandTerminated) {
	Rf_warning(_("unable to restore saved data in %s\n"), get_workspace_name());
	throw;
    }

    /* Initial Loading is done.
       At this point we try to invoke the .First Function.
       If there is an error we continue. */

    try {
	PROTECT(cmd = Rf_install(".First"));
	R_CurrentExpr = Rf_findVar(cmd, R_GlobalEnv);
	if (R_CurrentExpr != R_UnboundValue &&
	    TYPEOF(R_CurrentExpr) == CLOSXP) {
		PROTECT(R_CurrentExpr = Rf_lang1(cmd));
		R_CurrentExpr = Rf_eval(R_CurrentExpr, R_GlobalEnv);
		UNPROTECT(1);
	}
	UNPROTECT(1);
    }
    catch (CommandTerminated) {
      check_session_exit();
    }

    /* Try to invoke the .First.sys function, which loads the default packages.
       If there is an error we continue. */

    try {
	PROTECT(cmd = Rf_install(".First.sys"));
	R_CurrentExpr = Rf_findVar(cmd, baseEnv);
	if (R_CurrentExpr != R_UnboundValue &&
	    TYPEOF(R_CurrentExpr) == CLOSXP) {
		PROTECT(R_CurrentExpr = Rf_lang1(cmd));
#ifdef PROVENANCE_TRACKING
		ProvenanceTracker::CommandScope scope(R_CurrentExpr);
#endif		
		R_CurrentExpr = Rf_eval(R_CurrentExpr, R_GlobalEnv);
		UNPROTECT(1);
	}
	UNPROTECT(1);
    }
    catch (CommandTerminated) {
        check_session_exit();
    }

    {
	int i;
	for(i = 0 ; i < ndeferred_warnings; i++)
	    Rf_warning(deferred_warnings[i]);
    }
    if (R_CollectWarnings) {
	Rf_PrintWarnings(n_("Warning message during startup:", "Warning messages during startup:", R_CollectWarnings));
    }
    if(R_Verbose)
	REprintf(" ending setup_Rmainloop(): R_Interactive = %d {main.cpp}\n",
		 R_Interactive);

    R_Is_Running = 2;
}

extern SA_TYPE SaveAction; /* from src/main/startup.cpp */

static void end_Rmainloop(void)
{
    /* refrain from printing trailing '\n' in slave mode */
    if (!R_Slave)
	Rprintf("\n");
    /* run the .Last function. If it gives an error, will drop back to main
       loop. */
    R_CleanUp(SA_DEFAULT, 0, 1);
}

void run_Rmainloop(void)
{
    /* Here is the real R read-eval-loop. */
    /* We handle the console until end-of-file. */
    while (true) {
	try {
	    R_ReplConsole(R_GlobalEnv, 0);
            break;
	}
	catch (CommandTerminated) {
            check_session_exit();
            continue;
	}
    }
    end_Rmainloop(); /* must go here */
}

void Rf_mainloop(void)
{
    Evaluator evalr;
    setup_Rmainloop();
    run_Rmainloop();
}

/*this functionality now appears in 3
  places-jump_to_toplevel/profile/here */

static void printwhere(void)
{
  FunctionContext *cptr;
  int lct = 1;

  for (cptr = FunctionContext::innermost();
       cptr;
       cptr = FunctionContext::innermost(cptr->nextOut())) {
      if (TYPEOF(const_cast<Expression*>(cptr->call())) == LANGSXP) {
	  Rprintf("where %d", lct++);
	  Rf_SrcrefPrompt("", cptr->sourceLocation());
	  Rf_PrintValue(const_cast<Expression*>(cptr->call()));
      }
  }
  Rprintf("\n");
}

static void printBrowserHelp(void)
{
    Rprintf("n          next\n");
    Rprintf("s          step into\n");
    Rprintf("f          finish\n");
    Rprintf("c or cont  continue\n");
    Rprintf("Q          quit\n");
    Rprintf("where      show stack\n");
    Rprintf("help       show help\n");
    Rprintf("<expr>     evaluate expression\n");
}

static int ParseBrowser(SEXP CExpr, SEXP rho)
{
    int rval = 0;
    if (Rf_isSymbol(CExpr)) {
	const char *expr = R_CHAR(PRINTNAME(CExpr));
	if (streql(expr, "c") || streql(expr, "cont")) {
	    rval = 1;
	    SET_ENV_DEBUG(rho, FALSE);
#if 0
	} else if (streql(expr, "f")) {
	    rval = 1;
	    RCNTXT *cntxt = R_GlobalContext;
	    while (cntxt != R_ToplevelContext
		      && !(cntxt->callflag & (CTXT_RETURN | CTXT_LOOP))) {
		cntxt = cntxt->nextcontext;
	    }
	    cntxt->browserfinish = 1;
	    SET_ENV_DEBUG(rho, TRUE);
	    R_BrowserLastCommand = 'f';
#endif
	} else if (streql(expr, "help")) {
	    rval = 2;
	    printBrowserHelp();
	} else if (streql(expr, "n")) {
	    rval = 1;
	    SET_ENV_DEBUG(rho, TRUE);
	    R_BrowserLastCommand = 'n';
	} else if (streql(expr, "Q")) {

	    /* this is really dynamic state that should be managed as such */
	    SET_ENV_DEBUG(rho, FALSE); /*PR#1721*/

	    jump_to_toplevel();
	} else if (streql(expr, "s")) {
	    rval = 1;
	    SET_ENV_DEBUG(rho, TRUE);
	    R_BrowserLastCommand = 's';
	} else if (streql(expr, "where")) {
	    rval = 2;
	    printwhere();
	    /* SET_ENV_DEBUG(rho, TRUE); */
	} else if (streql(expr, "r")) {
	    SEXP hooksym = Rf_install(".tryResumeInterrupt");
	    if (SYMVALUE(hooksym) != R_UnboundValue) {
		Expression* hcall;
		R_Busy(1);
		PROTECT(hcall = new Expression(hooksym, { nullptr }));
		hcall->evaluate(Environment::global());
		UNPROTECT(1);
	    }
	}
    }

    return rval;
}

/* There's another copy of this in eval.cpp */
static void PrintCall(SEXP call, SEXP rho)
{
    int old_bl = R_BrowseLines,
	blines = Rf_asInteger(Rf_GetOption1(Rf_install("deparse.max.lines")));
    if(blines != NA_INTEGER && blines > 0)
	R_BrowseLines = blines;
    Rf_PrintValueRec(call, rho);
    R_BrowseLines = old_bl;
}

/* browser(text = "", condition = NULL, expr = TRUE, skipCalls = 0L)
 * ------- but also called from ./eval.cpp */
SEXP attribute_hidden do_browser(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int savestack;
    GCStackRoot<> topExp(R_CurrentExpr);
    RObject* ans = nullptr;

    /* Cannot call checkArity(op, args), because "op" may be a closure  */
    /* or a primitive other than "browser".  */

    /* argument matching */
    ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::EVALUATED);
    static GCRoot<ArgMatcher> matcher
	= new ArgMatcher({ "text", "condition", "expr", "skipcalls" });
    SEXP text, condition, expr, skipcalls;
    matcher->match(arglist, { &text, &condition, &expr, &skipcalls });

    /* substitute defaults */
    if (text == R_MissingArg)
	text = Rf_mkString("");
    if(condition == R_MissingArg)
	condition = R_NilValue;
    if(expr == R_MissingArg)
	expr = Rf_ScalarLogical(1);
    if(skipcalls == R_MissingArg)
	skipcalls = Rf_ScalarInteger(0);

    /* return if 'expr' is not TRUE */
    if( !Rf_asLogical(expr) ) {
	return R_NilValue;
    }

    // TODO: skipcalls isn't being used here.
    Browser browser(text, condition);

    /* Save the evaluator state information */
    /* so that it can be restored on exit. */

    savestack = int(ProtectStack::size());

    if (!ENV_DEBUG(rho)) {
	ClosureContext* cptr = R_GlobalContext();
	Rprintf("Called from: ");
	if( cptr ) {
	    PrintCall(const_cast<Expression*>(cptr->call()), rho);
 	    SET_ENV_DEBUG(cptr->workingEnvironment(), TRUE);
	} else
	    Rprintf("top level \n");

	R_BrowseLines = 0;
    }

    {
	Environment* envir = SEXP_downcast<Environment*>(rho);
	Environment::ReturnScope returnscope(envir);
	bool redo;
	do {
	    GCStackRoot<PairList> saved_handler_stack(R_HandlerStack);
	    redo = false;
	    try {
		ClosureContext* cptr = R_GlobalContext();
		// rho doesn't have a top-level context.  The
		// following test stops an error if browser() is
		// invoked at top level, but this workaround needs to
		// be reviewed when arr understands restarts better!
		if (cptr)
		    R_InsertRestartHandlers(cptr, "browser");
		R_ReplConsole(rho, savestack);
	    }
	    catch (ReturnException& rx) {
		if (rx.environment() != envir) {
		    R_HandlerStack = saved_handler_stack;
		    throw;
		}
		ans = rx.value();
	    }
	    catch (CommandTerminated) {
		R_Visible = FALSE;
		redo = true;
	    }
	    R_HandlerStack = saved_handler_stack;
	} while (redo);
    }

    /* Reset the interpreter state. */

    ProtectStack::restoreSize(savestack);
    R_CurrentExpr = topExp;
    return ans;
}

void R_dot_Last(void)
{
    SEXP cmd;

    /* Run the .Last function. */
    /* Errors here should kick us back into the repl. */

    Evaluator evalr;
    PROTECT(cmd = Rf_install(".Last"));
    R_CurrentExpr = Rf_findVar(cmd, R_GlobalEnv);
    if (R_CurrentExpr != R_UnboundValue && TYPEOF(R_CurrentExpr) == CLOSXP) {
	PROTECT(R_CurrentExpr = Rf_lang1(cmd));
	R_CurrentExpr = Rf_eval(R_CurrentExpr, R_GlobalEnv);
	UNPROTECT(1);
    }
    UNPROTECT(1);
    PROTECT(cmd = Rf_install(".Last.sys"));
    R_CurrentExpr = Rf_findVar(cmd, R_BaseNamespace);
    if (R_CurrentExpr != R_UnboundValue && TYPEOF(R_CurrentExpr) == CLOSXP) {
	PROTECT(R_CurrentExpr = Rf_lang1(cmd));
	R_CurrentExpr = Rf_eval(R_CurrentExpr, R_GlobalEnv);
	UNPROTECT(1);
    }
    UNPROTECT(1);
}

SEXP attribute_hidden do_quit(/*const*/ Expression* call, const BuiltInFunction* op, RObject* save_, RObject* status_, RObject* runLast_)
{
    const char *tmp;
    SA_TYPE ask=SA_DEFAULT;
    int status, runLast;

    /* if there are any browser contexts active don't quit */
    if(Browser::numberActive() > 0) {
	Rf_warning(_("cannot quit from browser"));
	return R_NilValue;
    }
    if( !Rf_isString(save_) )
	Rf_error(_("one of \"yes\", \"no\", \"ask\" or \"default\" expected."));
    tmp = R_CHAR(STRING_ELT(save_, 0)); /* ASCII */
    if( streql(tmp, "ask") ) {
	ask = SA_SAVEASK;
	if(!R_Interactive)
	    Rf_warning(_("save=\"ask\" in non-interactive use: command-line default will be used"));
    } else if( streql(tmp, "no") )
	ask = SA_NOSAVE;
    else if( streql(tmp, "yes") )
	ask = SA_SAVE;
    else if( streql(tmp, "default") )
	ask = SA_DEFAULT;
    else
	Rf_error(_("unrecognized value of 'save'"));
    status = Rf_asInteger(status_);
    if (status == NA_INTEGER) {
	Rf_warning(_("invalid 'status', 0 assumed"));
	status = 0;
    }
    runLast = Rf_asLogical(runLast_);
    if (runLast == NA_LOGICAL) {
	Rf_warning(_("invalid 'runLast', FALSE assumed"));
	runLast = 0;
    }
    /* run the .Last function. If it gives an error, will drop back to main
       loop. */
    R_CleanUp(ask, status, runLast);
    exit(0);
    /*NOTREACHED*/
}


#include <R_ext/Callbacks.h>

static R_ToplevelCallbackEl *Rf_ToplevelTaskHandlers = nullptr;

/**
  This is the C-level entry point for registering a handler
  that is to be called when each top-level task completes.

  Perhaps we need names to make removing them handlers easier
  since they could be more identified by an invariant (rather than
  position).
 */
R_ToplevelCallbackEl *
Rf_addTaskCallback(R_ToplevelCallback cb, void *data,
		   void (*finalizer)(void *), const char *name, int *pos)
{
    int which;
    R_ToplevelCallbackEl *el;
    el = static_cast<R_ToplevelCallbackEl *>(malloc(sizeof(R_ToplevelCallbackEl)));
    if(!el)
	Rf_error(_("cannot allocate space for toplevel callback element"));

    el->data = data;
    el->cb = cb;
    el->next = nullptr;
    el->finalizer = finalizer;

    if(Rf_ToplevelTaskHandlers == nullptr) {
	Rf_ToplevelTaskHandlers = el;
	which = 0;
    } else {
	R_ToplevelCallbackEl *tmp;
	tmp = Rf_ToplevelTaskHandlers;
	which = 1;
	while(tmp->next) {
	    which++;
	    tmp = tmp->next;
	}
	tmp->next = el;
    }

    if(!name) {
	char buf[20];
	snprintf(buf, 20, "%d", which+1);
	el->name = strdup(buf);
    } else
	el->name = strdup(name);

    if(pos)
	*pos = which;

    return(el);
}

Rboolean
Rf_removeTaskCallbackByName(const char *name)
{
    R_ToplevelCallbackEl *el = Rf_ToplevelTaskHandlers, *prev = nullptr;
    Rboolean status = TRUE;

    if(!Rf_ToplevelTaskHandlers) {
	return(FALSE); /* Rf_error("there are no task callbacks registered"); */
    }

    while(el) {
	if(streql(el->name, name)) {
	    if(prev == nullptr) {
		Rf_ToplevelTaskHandlers = el->next;
	    } else {
		prev->next = el->next;
	    }
	    break;
	}
	prev = el;
	el = el->next;
    }
    if(el) {
	if(el->finalizer)
	    el->finalizer(el->data);
	free(el->name);
	free(el);
    } else {
	status = FALSE;
    }
    return(status);
}

/**
  Remove the top-level task handler/callback identified by
  its position in the list of callbacks.
 */
Rboolean
Rf_removeTaskCallbackByIndex(int id)
{
    R_ToplevelCallbackEl *el = Rf_ToplevelTaskHandlers, *tmp = nullptr;
    Rboolean status = TRUE;

    if(id < 0)
	Rf_error(_("negative index passed to R_removeTaskCallbackByIndex"));

    if(Rf_ToplevelTaskHandlers) {
	if(id == 0) {
	    tmp = Rf_ToplevelTaskHandlers;
	    Rf_ToplevelTaskHandlers = Rf_ToplevelTaskHandlers->next;
	} else {
	    int i = 0;
	    while(el && i < (id-1)) {
		el = el->next;
		i++;
	    }

	    if(i == (id -1) && el) {
		tmp = el->next;
		el->next = (tmp ? tmp->next : nullptr);
	    }
	}
    }
    if(tmp) {
	if(tmp->finalizer)
	    tmp->finalizer(tmp->data);
	free(tmp->name);
	free(tmp);
    } else {
	status = FALSE;
    }

    return(status);
}


/**
  R-level entry point to remove an entry from the
  list of top-level callbacks. 'which' should be an
  integer and give us the 0-based index of the element
  to be removed from the list.

  @see Rf_RemoveToplevelCallbackByIndex(int)
 */
SEXP
R_removeTaskCallback(SEXP which)
{
    int id;
    Rboolean val;

    if(TYPEOF(which) == STRSXP) {
	if (LENGTH(which) == 0)
	    val = FALSE;
	else
	    val = Rf_removeTaskCallbackByName(R_CHAR(STRING_ELT(which, 0)));    } else {
	id = Rf_asInteger(which);
	if (id != NA_INTEGER) val = Rf_removeTaskCallbackByIndex(id - 1);
	else val = FALSE;
    }
    return Rf_ScalarLogical(val);
}

SEXP
R_getTaskCallbackNames(void)
{
    SEXP ans;
    R_ToplevelCallbackEl *el;
    int n = 0;

    el = Rf_ToplevelTaskHandlers;
    while(el) {
	n++;
	el = el->next;
    }
    PROTECT(ans = Rf_allocVector(STRSXP, n));
    n = 0;
    el = Rf_ToplevelTaskHandlers;
    while(el) {
	SET_STRING_ELT(ans, n, Rf_mkChar(el->name));
	n++;
	el = el->next;
    }
    UNPROTECT(1);
    return(ans);
}

/**
  Invokes each of the different handlers giving the
  top-level expression that was just evaluated,
  the resulting value from the evaluation, and
  whether the task succeeded. The last may be useful
  if a handler is also called as part of the error handling.
  We also have information about whether the result was printed or not.
  We currently do not pass this to the handler.
 */

  /* Flag to ensure that the top-level handlers aren't called recursively.
     Simple state to indicate that they are currently being run. */
static Rboolean Rf_RunningToplevelHandlers = FALSE;

/* This is not used in R and in no header */
void
Rf_callToplevelHandlers(SEXP expr, SEXP value, Rboolean succeeded,
			Rboolean visible)
{
    R_ToplevelCallbackEl *h, *prev = nullptr;
    Rboolean again;

    if(Rf_RunningToplevelHandlers == TRUE)
	return;

    h = Rf_ToplevelTaskHandlers;
    Rf_RunningToplevelHandlers = TRUE;
    while(h) {
	again = (h->cb)(expr, value, succeeded, visible, h->data);
	if(R_CollectWarnings) {
	    REprintf(_("warning messages from top-level task callback '%s'\n"),
		     h->name);
	    Rf_PrintWarnings(nullptr);
	}
	if(again) {
	    prev = h;
	    h = h->next;
	} else {
	    R_ToplevelCallbackEl *tmp;
	    tmp = h;
	    if(prev)
		prev->next = h->next;
	    h = h->next;
	    if(tmp == Rf_ToplevelTaskHandlers)
		Rf_ToplevelTaskHandlers = h;
	    if(tmp->finalizer)
		tmp->finalizer(tmp->data);
	    free(tmp);
	}
    }

    Rf_RunningToplevelHandlers = FALSE;
}


Rboolean
R_taskCallbackRoutine(SEXP expr, SEXP value, Rboolean succeeded,
		      Rboolean visible, void *userData)
{
    SEXP f = static_cast<SEXP>(userData);
    SEXP e, tmp, val, cur;
    int errorOccurred;
    Rboolean again, useData = Rboolean(LOGICAL(VECTOR_ELT(f, 2))[0]);

    PROTECT(e = Rf_allocVector(LANGSXP, 5 + useData));
    SETCAR(e, VECTOR_ELT(f, 0));
    cur = CDR(e);
    SETCAR(cur, tmp = Rf_allocVector(LANGSXP, 2));
	SETCAR(tmp, R_QuoteSymbol);
	SETCAR(CDR(tmp), expr);
    cur = CDR(cur);
    SETCAR(cur, value);
    cur = CDR(cur);
    SETCAR(cur, Rf_ScalarLogical(succeeded));
    cur = CDR(cur);
    SETCAR(cur, tmp = Rf_ScalarLogical(visible));
    if(useData) {
	cur = CDR(cur);
	SETCAR(cur, VECTOR_ELT(f, 1));
    }

    val = R_tryEval(e, nullptr, &errorOccurred);
    if(!errorOccurred) {
	PROTECT(val);
	if(TYPEOF(val) != LGLSXP) {
	      /* It would be nice to identify the function. */
	    Rf_warning(_("top-level task callback did not return a logical value"));
	}
	again = Rboolean(Rf_asLogical(val));
	UNPROTECT(1);
    } else {
	/* Rf_warning("error occurred in top-level task callback\n"); */
	again = FALSE;
    }
    return(again);
}

SEXP
R_addTaskCallback(SEXP f, SEXP data, SEXP useData, SEXP name)
{
    SEXP internalData;
    SEXP index;
    R_ToplevelCallbackEl *el;
    const char *tmpName = nullptr;

    internalData = Rf_allocVector(VECSXP, 3);
    R_PreserveObject(internalData);
    SET_VECTOR_ELT(internalData, 0, f);
    SET_VECTOR_ELT(internalData, 1, data);
    SET_VECTOR_ELT(internalData, 2, useData);

    if(Rf_length(name))
	tmpName = R_CHAR(STRING_ELT(name, 0));

    PROTECT(index = Rf_allocVector(INTSXP, 1));
    el = Rf_addTaskCallback(R_taskCallbackRoutine,  internalData,
			    reinterpret_cast<void (*)(void*)>(R_ReleaseObject), tmpName,
			    INTEGER(index));

    if(Rf_length(name) == 0) {
	PROTECT(name = Rf_mkString(el->name));
	Rf_setAttrib(index, R_NamesSymbol, name);
	UNPROTECT(1);
    } else {
	Rf_setAttrib(index, R_NamesSymbol, name);
    }

    UNPROTECT(1);
    return(index);
}

#undef __MAIN__

#ifndef Win32
/* this is here solely to pull in xxxpr.o */
#include <R_ext/RS.h>
extern "C" {
    void F77_SYMBOL(intpr) (const char *, int *, int *, int *);
}

void attribute_hidden dummy12345(void)
{
    int i = 0;
    F77_CALL(intpr)("dummy", &i, &i, &i);
}

#endif
