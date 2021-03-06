/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2014  The R Core Team
 *  Copyright (C) 2003	      The R Foundation
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
# include <config.h>
#endif

#include <Defn.h>
#include <Localization.h>
#include <Internal.h>
#include <ctype.h> /* for tolower */
#include <string.h>
#include <errno.h>

#include <Rmath.h>
#include <R_ext/RS.h>
#include <boost/preprocessor.hpp>

#include <rho/ClosureContext.hpp>
#include <rho/RAllocStack.hpp>


using namespace rho;
using namespace std;

/* Was 'name' prior to 2.13.0, then .NAME, but checked as
   'name' up to 2.15.1. */
static void check1arg2(SEXP arg, SEXP call, const char *formal)
{
    if (TAG(arg) == nullptr) return;
    Rf_errorcall(call, "the first argument should not be named");
 }



/* These are set during the first call to do_dotCode() below. */

static GCRoot<> NaokSymbol = nullptr;
static GCRoot<> DupSymbol = nullptr;
static GCRoot<> PkgSymbol = nullptr;
static GCRoot<> EncSymbol = nullptr;
static GCRoot<> CSingSymbol = nullptr;

#include <Rdynpriv.h>

enum DLLType {NOT_DEFINED, FILENAME, DLL_HANDLE, R_OBJECT};
struct DllReference {
    char DLLname[PATH_MAX];
    HINSTANCE dll;
    SEXP  obj;
    DLLType type;
};

/* Maximum length of entry-point name, including nul terminator */
constexpr size_t MaxSymbolBytes = 1024;

/* Maximum number of args to .C, .Fortran and .Call */
#define MAX_ARGS 65

/* This looks up entry points in DLLs in a platform specific way. */
static DL_FUNC R_FindNativeSymbolFromDLL(char *name, DllReference *dll,
			  R_RegisteredNativeSymbol *symbol, SEXP env);

static SEXP naokfind(SEXP args, int * len, int *naok, DllReference *dll);
static SEXP pkgtrim(SEXP args, DllReference *dll);

R_INLINE static Rboolean isNativeSymbolInfo(SEXP op)
{
    /* was: Rf_inherits(op, "NativeSymbolInfo")
     * Rf_inherits() is slow because of string comparisons, so use
     * structural check instead. */
    return Rboolean((TYPEOF(op) == VECSXP &&
		     LENGTH(op) >= 2 &&
		     TYPEOF(VECTOR_ELT(op, 1)) == EXTPTRSXP));
}

/*
  Called from resolveNativeRoutine (and itself).

  Checks whether the specified object correctly identifies a native routine.
  op is the supplied value for .NAME.  This can be
   a) a string (when this does nothing).
   b) an external pointer giving the address of the routine
      (e.g. getNativeSymbolInfo("foo")$address)
   c) or a NativeSymbolInfo itself  (e.g. getNativeSymbolInfo("foo"))

   It copies the symbol name to buf.

   NB: in the last two cases it sets fun and symbol as well!
 */
static void checkValidSymbolId(SEXP op, SEXP call, DL_FUNC* fun,
    R_RegisteredNativeSymbol* symbol, char* buf)
{
    if (Rf_isValidString(op)) return;

    if(TYPEOF(op) == EXTPTRSXP) {
	static SEXP native_symbol = nullptr;
	static SEXP registered_native_symbol = nullptr;
	if (native_symbol == nullptr) {
	    native_symbol = Rf_install("native symbol");
	    registered_native_symbol = Rf_install("registered native symbol");
	}
	char *p = nullptr;
	if(R_ExternalPtrTag(op) == native_symbol)
	   *fun = R_ExternalPtrAddrFn(op);
	else if(R_ExternalPtrTag(op) == registered_native_symbol) {
	   R_RegisteredNativeSymbol *tmp;
	   tmp = static_cast<R_RegisteredNativeSymbol *>(R_ExternalPtrAddr(op));
	   if(tmp) {
	      if(symbol->type != R_ANY_SYM && symbol->type != tmp->type)
		 Rf_errorcall(call, _("NULL value passed as symbol address"));
		/* Check the type of the symbol. */
	      switch(symbol->type) {
	      case R_C_SYM:
		  *fun = tmp->symbol.c->fun;
		  p = tmp->symbol.c->name;
		  break;
	      case R_CALL_SYM:
		  *fun = tmp->symbol.call->fun;
		  p = tmp->symbol.call->name;
		  break;
	      case R_FORTRAN_SYM:
		  *fun = tmp->symbol.fortran->fun;
		  p = tmp->symbol.fortran->name;
		  break;
	      case R_EXTERNAL_SYM:
		  *fun = tmp->symbol.external->fun;
		  p = tmp->symbol.external->name;
		  break;
	      default:
		 /* Something unintended has happened if we get here. */
		  Rf_errorcall(call, _("Unimplemented type %d in createRSymbolObject"),
			    symbol->type);
		  break;
	      }
	      *symbol = *tmp;
	   }
	}
	/* This is illegal C */
	if(*fun == nullptr)
	    Rf_errorcall(call, _("NULL value passed as symbol address"));

	/* copy the symbol name. */
	if (p && buf) {
	    if (strlen(p) >= MaxSymbolBytes)
		Rf_error(_("symbol '%s' is too long"), p);
	    memcpy(buf, p, strlen(p)+1);
	}

	return;
    }
    else if(isNativeSymbolInfo(op)) {
	checkValidSymbolId(VECTOR_ELT(op, 1), call, fun, symbol, buf);
	return;
    }

    Rf_errorcall(call,
      _("first argument must be a string (of length 1) or native symbol reference"));
    return; /* not reached */
}

HIDDEN
DL_FUNC R_dotCallFn(SEXP op, SEXP call, int nargs) {
    R_RegisteredNativeSymbol symbol = {R_CALL_SYM, {nullptr}, nullptr};
    DL_FUNC fun = nullptr;
    checkValidSymbolId(op, call, &fun, &symbol, nullptr);
    /* should check arg count here as well */
    return fun;
}

/*
  This is the routine that is called by do_dotCode, do_dotcall and
  do_External to find the DL_FUNC to invoke. It handles processing the
  arguments for the PACKAGE argument, if present, and also takes care
  of the cases where we are given a NativeSymbolInfo object, an
  address directly, and if the DLL is specified. If no PACKAGE is
  provided, we check whether the calling function is in a namespace
  and look there.
*/

static SEXP resolveNativeRoutine(SEXP args, DL_FUNC *fun,
		     R_RegisteredNativeSymbol *symbol, char *buf,
		     int *nargs, int *naok, SEXP call, SEXP env)
{
    SEXP op;
    const char *p; char *q;
    DllReference dll;
    /* This is used as shorthand for 'all' in R_FindSymbol, but
       should never be supplied */
    strcpy(dll.DLLname, "");
    dll.dll = nullptr; dll.obj = nullptr; dll.type = NOT_DEFINED;

    op = CAR(args);  // value of .NAME =
    /* NB, this sets fun, symbol and buf and is not just a check! */
    checkValidSymbolId(op, call, fun, symbol, buf);

    /* The following code modifies the argument list */
    /* We know this is ok because do_dotCode is entered */
    /* with its arguments evaluated. */

    if(symbol->type == R_C_SYM || symbol->type == R_FORTRAN_SYM) {
	/* And that also looks for PACKAGE = */
	args = naokfind(CDR(args), nargs, naok, &dll);
	if(*naok == R_NaLog)
	    Rf_errorcall(call, _("invalid '%s' value"), "naok");
	if(*nargs > MAX_ARGS)
	    Rf_errorcall(call, _("too many arguments in foreign function call"));
    } else {
	/* This has the side effect of setting dll.type if a PACKAGE=
	   argument if found, but it will only be used if a string was
	   passed in  */
	args = pkgtrim(args, &dll);
    }

    /* We were given a symbol (or an address), so we are done. */
    if (*fun) return args;

    if (dll.type == FILENAME && !strlen(dll.DLLname))
	Rf_errorcall(call, _("PACKAGE = \"\" is invalid"));

    // find if we were called from a namespace
    SEXP env2 = ENCLOS(env);
    const char *ns = "";
    if(R_IsNamespaceEnv(env2))
	ns = R_CHAR(STRING_ELT(R_NamespaceEnvSpec(env2), 0));
    else env2 = nullptr;

#ifdef CHECK_CROSS_USAGE
    if (dll.type == FILENAME && strcmp(dll.DLLname, "base")) {
	if(strlen(ns) && strcmp(dll.DLLname, ns) &&
	   !(streql(dll.DLLname, "BioC_graph") && streql(ns, "graph")))
	    Rf_warningcall(call,
			"using PACKAGE = \"%s\" from namespace '%s'",
			dll.DLLname, ns);
    }
#endif

    /* Make up the load symbol */
    if(TYPEOF(op) == STRSXP) {
	const void *vmax = vmaxget();
	p = Rf_translateChar(STRING_ELT(op, 0));
	if(strlen(p) >= MaxSymbolBytes)
	    Rf_error(_("symbol '%s' is too long"), p);
	q = buf;
	while ((*q = *p) != '\0') {
	    if(symbol->type == R_FORTRAN_SYM) *q = char(tolower(*q));
	    p++;
	    q++;
	}
	vmaxset(vmax);
    }

    if(dll.type != FILENAME && strlen(ns)) {
	/* no PACKAGE= arg, so see if we can identify a DLL
	   from the namespace defining the function */
	*fun = R_FindNativeSymbolFromDLL(buf, &dll, symbol, env2);
	if (*fun) return args;
	Rf_errorcall(call, "\"%s\" not resolved from current namespace (%s)",
		  buf, ns);
    }

    /* NB: the actual conversion to the symbol is done in
       R_dlsym in Rdynload.cpp.  That prepends an underscore (usually),
       and may append one or more underscores.
    */

    *fun = R_FindSymbol(buf, dll.DLLname, symbol);
    if (*fun) return args;

    /* so we've failed and bail out */
    if(strlen(dll.DLLname)) {
	switch(symbol->type) {
	case R_C_SYM:
	    Rf_errorcall(call,
		      _("\"%s\" not available for %s() for package \"%s\""),
		      buf, ".C", dll.DLLname);
	    break;
	case R_FORTRAN_SYM:
	    Rf_errorcall(call,
		      _("\"%s\" not available for %s() for package \"%s\""),
		      buf, ".Fortran", dll.DLLname);
	    break;
	case R_CALL_SYM:
	    Rf_errorcall(call,
		      _("\"%s\" not available for %s() for package \"%s\""),
		      buf, ".Call", dll.DLLname);
	    break;
	case R_EXTERNAL_SYM:
	    Rf_errorcall(call,
		      _("\"%s\" not available for %s() for package \"%s\""),
		      buf, ".External", dll.DLLname);
	    break;
	case R_ANY_SYM:
	    Rf_errorcall(call,
		      _("%s symbol name \"%s\" not in DLL for package \"%s\""),
		      "C/Fortran", buf, dll.DLLname);
	    break;
	}
    } else
	Rf_errorcall(call, _("%s symbol name \"%s\" not in load table"),
		  symbol->type == R_FORTRAN_SYM ? "Fortran" : "C", buf);

    return args; /* -Wall */
}

static Rboolean checkNativeType(int targetType, int actualType)
{
    if(targetType > 0) {
	if(targetType == INTSXP || targetType == LGLSXP) {
	    return(Rboolean(actualType == INTSXP || actualType == LGLSXP));
	}
	return(Rboolean(targetType == actualType));
    }

    return TRUE;
}

static Rboolean comparePrimitiveTypes(R_NativePrimitiveArgType type, SEXP s)
{
   SEXPTYPE stype = SEXPTYPE(type);
   if(stype == ANYSXP || TYPEOF(s) == stype)
      return TRUE;

   if(type == SINGLESXP)
      return(Rboolean(Rf_asLogical(Rf_getAttrib(s, Rf_install("Csingle"))) == TRUE));

   return FALSE;
}

/* Foreign Function Interface.  This code allows a user to call C */
/* or Fortran code which is either statically or dynamically linked. */

/* NB: this leaves NAOK and DUP arguments on the list */

/* find NAOK and DUP, find and remove PACKAGE */
static SEXP naokfind(SEXP args, int * len, int *naok, DllReference *dll)
{
    SEXP s, prev;
    int nargs=0, naokused=0, dupused=0, pkgused=0;
    const char *p;

    *naok = 0;
    *len = 0;
    for(s = args, prev=args; s != nullptr;) {
	if(TAG(s) == NaokSymbol) {
	    *naok = Rf_asLogical(CAR(s));
	    if(naokused++ == 1) Rf_warning(_("'%s' used more than once"), "NAOK");
	} else if(TAG(s) == DupSymbol) {
	    if(dupused++ == 1) Rf_warning(_("'%s' used more than once"), "DUP");
	} else if(TAG(s) == PkgSymbol) {
	    dll->obj = CAR(s);  // really?
	    if(TYPEOF(CAR(s)) == STRSXP) {
		p = Rf_translateChar(STRING_ELT(CAR(s), 0));
		if(strlen(p) > PATH_MAX - 1)
		    Rf_error(_("DLL name is too long"));
		dll->type = FILENAME;
		strcpy(dll->DLLname, p);
		if(pkgused++ > 1)
		    Rf_warning(_("'%s' used more than once"), "PACKAGE");
		/* More generally, this should allow us to process
		   any additional arguments and not insist that PACKAGE
		   be the last argument.
		*/
	    } else {
		/* Have a DLL object, which is not something documented .... */
		if(TYPEOF(CAR(s)) == EXTPTRSXP) {
		    dll->dll = static_cast<HINSTANCE>(R_ExternalPtrAddr(CAR(s)));
		    dll->type = DLL_HANDLE;
		} else if(TYPEOF(CAR(s)) == VECSXP) {
		    dll->type = R_OBJECT;
		    dll->obj = s;
		    strcpy(dll->DLLname,
			   Rf_translateChar(STRING_ELT(VECTOR_ELT(CAR(s), 1), 0)));
		    dll->dll = static_cast<HINSTANCE>(R_ExternalPtrAddr(VECTOR_ELT(s, 4)));
		} else
		    Rf_error("incorrect type (%s) of PACKAGE argument\n",
			  Rf_type2char(TYPEOF(CAR(s))));
	    }
	} else {
	    nargs++;
	    prev = s;
	    s = CDR(s);
	    continue;
	}
	if(s == args)
	    args = s = CDR(s);
	else
	    SETCDR(prev, s = CDR(s));
    }
    *len = nargs;
    return args;
}

static void setDLLname(SEXP s, char *DLLname)
{
    SEXP ss = CAR(s);
    const char *name;

    if(TYPEOF(ss) != STRSXP || Rf_length(ss) != 1)
	Rf_error(_("PACKAGE argument must be a single character string"));
    name = Rf_translateChar(STRING_ELT(ss, 0));
    /* allow the package: form of the name, as returned by find */
    if(streqln(name, "package:", 8))
	name += 8;
    if(strlen(name) > PATH_MAX - 1)
	Rf_error(_("PACKAGE argument is too long"));
    strcpy(DLLname, name);
}

static SEXP pkgtrim(SEXP args, DllReference *dll)
{
    SEXP s, ss;
    int pkgused = 0;

    if (PkgSymbol == nullptr) PkgSymbol = Rf_install("PACKAGE");

    for(s = args ; s != nullptr;) {
	ss = CDR(s);
	/* Look for PACKAGE=. We look at the next arg, unless
	   this is the last one (which will only happen for one arg),
	   and remove it */
	if(ss == nullptr && TAG(s) == PkgSymbol) {
	    if(pkgused++ == 1)
		Rf_warning(_("'%s' used more than once"), "PACKAGE");
	    setDLLname(s, dll->DLLname);
	    dll->type = FILENAME;
	    return nullptr;
	}
	if(TAG(ss) == PkgSymbol) {
	    if(pkgused++ == 1)
		Rf_warning(_("'%s' used more than once"), "PACKAGE");
	    setDLLname(ss, dll->DLLname);
	    dll->type = FILENAME;
	    SETCDR(s, CDR(ss));
	}
	s = CDR(s);
    }
    return args;
}

static SEXP enctrim(SEXP args)
{
    SEXP s, ss;

    for(s = args ; s != nullptr;) {
	ss = CDR(s);
	/* Look for ENCODING=. We look at the next arg, unless
	   this is the last one (which will only happen for one arg),
	   and remove it */
	if(ss == nullptr && TAG(s) == EncSymbol) {
	    Rf_warning("ENCODING is defunct and will be ignored");
	    return nullptr;
	}
	if(TAG(ss) == EncSymbol) {
	    Rf_warning("ENCODING is defunct and will be ignored");
	    SETCDR(s, CDR(ss));
	}
	s = CDR(s);
    }
    return args;
}

HIDDEN SEXP do_isloaded(SEXP call, SEXP op, SEXP args, SEXP env)
{
    const char *sym, *type="", *pkg = "";
    int val = 1, nargs = Rf_length(args);
    R_RegisteredNativeSymbol symbol = {R_ANY_SYM, {nullptr}, nullptr};

    if (nargs < 1) Rf_error(_("no arguments supplied"));
    if (nargs > 3) Rf_error(_("too many arguments"));

    if(!Rf_isValidString(CAR(args)))
	Rf_error(_("invalid '%s' argument"), "symbol");
    sym = Rf_translateChar(STRING_ELT(CAR(args), 0));
    if(nargs >= 2) {
	if(!Rf_isValidString(CADR(args)))
	    Rf_error(_("invalid '%s' argument"), "PACKAGE");
	pkg = Rf_translateChar(STRING_ELT(CADR(args), 0));
    }
    if(nargs >= 3) {
	if(!Rf_isValidString(CADDR(args)))
	    Rf_error(_("invalid '%s' argument"), "type");
	type = R_CHAR(STRING_ELT(CADDR(args), 0)); /* ASCII */
	if(streql(type, "C")) symbol.type = R_C_SYM;
	else if(streql(type, "Fortran")) symbol.type = R_FORTRAN_SYM;
	else if(streql(type, "Call")) symbol.type = R_CALL_SYM;
	else if(streql(type, "External")) symbol.type = R_EXTERNAL_SYM;
    }
    if(!(R_FindSymbol(sym, pkg, &symbol))) val = 0;
    return Rf_ScalarLogical(val);
}

/*   Call dynamically loaded "internal" functions.
     Original code by Jean Meloche <jean@stat.ubc.ca> */

typedef SEXP (*R_ExternalRoutine)(SEXP);
typedef SEXP (*R_ExternalRoutine2)(SEXP, SEXP, SEXP, SEXP);

HIDDEN SEXP do_External(SEXP call, SEXP op, SEXP args, SEXP env)
{
    DL_FUNC ofun = nullptr;
    SEXP retval;
    R_RegisteredNativeSymbol symbol = {R_EXTERNAL_SYM, {nullptr}, nullptr};
    const void *vmax = vmaxget();
    char buf[MaxSymbolBytes];

    if (Rf_length(args) < 1) Rf_errorcall(call, _("'.NAME' is missing"));
    check1arg2(args, call, ".NAME");
    args = resolveNativeRoutine(args, &ofun, &symbol, buf, nullptr, nullptr,
				call, env);

    if(symbol.symbol.external && symbol.symbol.external->numArgs > -1) {
	int nargs = Rf_length(args) - 1;
	if(symbol.symbol.external->numArgs != nargs)
	    Rf_errorcall(call,
		      _("Incorrect number of arguments (%d), expecting %d for '%s'"),
		      nargs, symbol.symbol.external->numArgs, buf);
    }

    if (PRIMVAL(op) == 1) {
	R_ExternalRoutine2 fun = R_ExternalRoutine2(ofun);
	retval = fun(call, op, args, env);
    } else {
	R_ExternalRoutine fun = R_ExternalRoutine(ofun);
	retval = fun(args);
    }
    vmaxset(vmax);
    return retval;
}

typedef SEXP (*VarFun)(...);

HIDDEN SEXP R_doDotCall(DL_FUNC ofun, int nargs, SEXP *cargs,
				  SEXP call) {
    VarFun fun = nullptr;
    SEXP retval = nullptr;	/* -Wall */
    fun = (VarFun) ofun;
    switch (nargs) {
    case 0:
	retval = static_cast<SEXP>(ofun());
	break;

/*  This macro expands out to:
    case 1:
	retval = fun(cargs[0]);
	break;
    case 2:
	retval = fun(cargs[0], cargs[1]);
	break;
    ... on to case 65
*/

#define ARGUMENT_LIST(Z, N, IGNORED) BOOST_PP_COMMA_IF(N) cargs[N]
#define CASE_STATEMENT(Z, N, IGNORED)                                       \
    case N:                                                                 \
	retval = fun(BOOST_PP_REPEAT(N, ARGUMENT_LIST, 0)); \
	break;

	BOOST_PP_REPEAT_FROM_TO(1, BOOST_PP_INC(MAX_ARGS), CASE_STATEMENT, 0);

#undef ARGUMENT_LIST
#undef CASE_STATEMENT

    default:
	Rf_errorcall(call, _("too many arguments, sorry"));
    }
    return retval;
}

/* .Call(name, <args>) */
HIDDEN SEXP do_dotcall(SEXP call, SEXP op, SEXP args, SEXP env)
{
    DL_FUNC ofun = nullptr;
    SEXP retval, cargs[MAX_ARGS], pargs;
    R_RegisteredNativeSymbol symbol = {R_CALL_SYM, {nullptr}, nullptr};

    int nargs;
    const void *vmax = vmaxget();
    char buf[MaxSymbolBytes];

    if (Rf_length(args) < 1) Rf_errorcall(call, _("'.NAME' is missing"));
    check1arg2(args, call, ".NAME");

    args = resolveNativeRoutine(args, &ofun, &symbol, buf, nullptr, nullptr, call, env);
    args = CDR(args);

    for(nargs = 0, pargs = args ; pargs != nullptr; pargs = CDR(pargs)) {
	if (nargs == MAX_ARGS)
	    Rf_errorcall(call, _("too many arguments in foreign function call"));
	cargs[nargs] = CAR(pargs);
	nargs++;
    }
    if(symbol.symbol.call && symbol.symbol.call->numArgs > -1) {
	if(symbol.symbol.call->numArgs != nargs)
	    Rf_errorcall(call,
		      _("Incorrect number of arguments (%d), expecting %d for '%s'"),
		      nargs, symbol.symbol.call->numArgs, buf);
    }

    if (R_check_constants < 4)
	retval = R_doDotCall(ofun, nargs, cargs, call);
    else {
	SEXP *cargscp = (SEXP *) R_alloc(nargs, sizeof(SEXP));
	int i;
	for(i = 0; i < nargs; i++)
	    cargscp[i] = PROTECT(Rf_duplicate(cargs[i]));
	retval = PROTECT(R_doDotCall(ofun, nargs, cargs, call));
	Rboolean constsOK = TRUE;
	for(i = 0; constsOK && i < nargs; i++)
	    /* 39: not numerical comparison, not single NA, not attributes as
               set, do ignore byte-code, do ignore environments of closures,
               not ignore srcref

               srcref is not ignored because ignoring it is expensive
               (it triggers duplication)
	    */
            if (!R_compute_identical(cargs[i], cargscp[i], 39)
		    && !R_checkConstants(FALSE))
		constsOK = FALSE;
	if (!constsOK) {
	    REprintf("ERROR: detected compiler constant(s) modification after .Call invocation of function %s from library %s (%s).\n",
		buf,
		symbol.dll ? symbol.dll->name : "unknown",
		symbol.dll ? symbol.dll->path : "unknown");
	    for(i = 0; i < nargs; i++)
		if (!R_compute_identical(cargs[i], cargscp[i], 39))
		    REprintf("NOTE: .Call function %s modified its argument (number %d, type %s, length %d)\n",
			buf,
			i + 1,
			R_CHAR(Rf_type2str(TYPEOF(cargscp[i]))),
			Rf_length(cargscp[i])
		    );
	    R_Suicide("compiler constants were modified (in .Call?)!\n");
	}
	UNPROTECT(nargs + 1);
    }
    vmaxset(vmax);
    return retval;
}

/*  Call dynamically loaded "internal" graphics functions
    .External.graphics (used in graphics) and  .Call.graphics (used in grid).

    If there is an error or user-interrupt in the above
    evaluation, dd->recordGraphics is set to TRUE
    on all graphics devices (see GEonExit(); called in errors.cpp)

    NOTE: if someone uses try() around this call and there
    is an error, then dd->recordGraphics stays FALSE, so
    subsequent pages of graphics output are NOT saved on
    the display list.  A workaround is to deliberately
    force an error in a graphics call (e.g., a grid popViewport()
    while in the ROOT viewport) which will reset dd->recordGraphics
    to TRUE as per the comment above.
*/

#include <R_ext/GraphicsEngine.h>

HIDDEN SEXP do_Externalgr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP retval;
    GEDevDesc* dd = GEcurrentDevice();
    Rboolean record = dd->recordGraphics;
    dd->recordGraphics = FALSE;
    PROTECT(retval = do_External(call, op, args, env));
    dd->recordGraphics = record;
    if (GErecording(call, dd)) { // which is record && call != nullptr
	if (!GEcheckState(dd))
	    Rf_errorcall(call, _("invalid graphics state"));
	GErecordGraphicOperation(op, args, dd);
    }
    UNPROTECT(1);
    return retval;
}

HIDDEN SEXP do_dotcallgr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP retval;
    GEDevDesc* dd = GEcurrentDevice();
    Rboolean record = dd->recordGraphics;
    dd->recordGraphics = FALSE;
    PROTECT(retval = do_dotcall(call, op, args, env));
    dd->recordGraphics = record;
    if (GErecording(call, dd)) {
	if (!GEcheckState(dd))
	    Rf_errorcall(call, _("invalid graphics state"));
	GErecordGraphicOperation(op, args, dd);
    }
    UNPROTECT(1);
    return retval;
}

static SEXP
Rf_getCallingDLL(void)
{
    SEXP e, ans;
    SEXP rho = nullptr;
    Rboolean found = FALSE;

    /* First find the environment of the caller.
       Testing shows this is the right caller, despite the .C/.Call ...
     */
    {
	ClosureContext* cptr = R_GlobalContext();
	if (cptr)
	    rho = cptr->workingEnvironment();
    }

    /* Then search up until we hit a namespace or globalenv.
       The idea is that we will not find a namespace unless the caller
       was defined in one. */
    while(rho != nullptr) {
	if (rho == R_GlobalEnv) break;
	else if (R_IsNamespaceEnv(rho)) {
	    found = TRUE;
	    break;
	}
	rho = ENCLOS(rho);
    }
    if(!found) return nullptr;

    PROTECT(e = Rf_lang2(Rf_install("getCallingDLLe"), rho));
    ans = Rf_eval(e,  R_GlobalEnv);
    UNPROTECT(1);
    return(ans);
}


/*
  We are given the PACKAGE argument in dll.obj
  and we can try to figure out how to resolve this.
  0) dll.obj is NULL.  Then find the environment of the
   calling function and if it is a namespace, get the first registered DLL.

  1) dll.obj is a DLLInfo object
*/
static DL_FUNC
R_FindNativeSymbolFromDLL(char *name, DllReference *dll,
			  R_RegisteredNativeSymbol *symbol,
			  SEXP env)
{
    int numProtects = 0;
    DllInfo *info;
    DL_FUNC fun = nullptr;

    if(dll->obj == nullptr) {
	/* Rprintf("\nsearching for %s\n", name); */
	//dll->obj = Rf_getCallingDLL();
	if (env != nullptr) {
	    SEXP e;
	    PROTECT(e = Rf_lang2(Rf_install("getCallingDLLe"), env));
	    dll->obj = Rf_eval(e, R_GlobalEnv);
	    UNPROTECT(1);
	} else dll->obj = Rf_getCallingDLL();
	PROTECT(dll->obj); numProtects++;
    }

    if(Rf_inherits(dll->obj, "DLLInfo")) {
	SEXP tmp;
	tmp = VECTOR_ELT(dll->obj, 4);
	info = static_cast<DllInfo *>(R_ExternalPtrAddr(tmp));
	if(!info)
	    Rf_error(_("NULL value for DLLInfoReference when looking for DLL"));
	fun = R_dlsym(info, name, symbol);
    }

    if(numProtects) UNPROTECT(numProtects);

    return fun;
}



/* .C() {op=0}  or  .Fortran() {op=1} */
/* Use of this except for atomic vectors is not allowed for .Fortran,
   and is only kept for legacy code for .C.

   CRAN packages R2Cuba, RCALI, ars, coxme, fCopulae, locfit, nlme,
   splinesurv and survival pass functions, the case of RCALI as a list
   of two functions.

   RecordLinkage and locfit pass lists.
*/

/* pattern and number of guard bytes */
#define FILL 0xee
#define NG 64

HIDDEN SEXP do_dotCode(SEXP call, SEXP op, SEXP args, SEXP env)
{
    void **cargs, **cargs0 = nullptr /* -Wall */;
    int naok, na, nargs, Fort;
    Rboolean havenames, copy = R_CBoundsCheck; /* options(CboundsCheck) */
    DL_FUNC ofun = nullptr;
    VarFun fun = nullptr;
    SEXP ans, pa, s;
    R_RegisteredNativeSymbol symbol = {R_C_SYM, {nullptr}, nullptr};
    R_NativePrimitiveArgType *checkTypes = nullptr;
    const void *vmax;
    char symName[MaxSymbolBytes];

    if (Rf_length(args) < 1) Rf_errorcall(call, _("'.NAME' is missing"));
    check1arg2(args, call, ".NAME");
    if (NaokSymbol == nullptr || DupSymbol == nullptr || PkgSymbol == nullptr) {
	NaokSymbol = Rf_install("NAOK");
	DupSymbol = Rf_install("DUP");
	PkgSymbol = Rf_install("PACKAGE");
    }
    if (EncSymbol == nullptr) EncSymbol = Rf_install("ENCODING");
    if (CSingSymbol == nullptr) CSingSymbol = Rf_install("Csingle");
    vmax = vmaxget();
    Fort = PRIMVAL(op);
    if(Fort) symbol.type = R_FORTRAN_SYM;

    args = enctrim(args);
    args = resolveNativeRoutine(args, &ofun, &symbol, symName, &nargs,
				&naok, call, env);
    fun = reinterpret_cast<VarFun>(ofun);

    if(symbol.symbol.c && symbol.symbol.c->numArgs > -1) {
	if(symbol.symbol.c->numArgs != nargs)
	    Rf_errorcall(call,
		      _("Incorrect number of arguments (%d), expecting %d for '%s'"),
		      nargs, symbol.symbol.c->numArgs, symName);

	checkTypes = symbol.symbol.c->types;
    }

    /* Construct the return value */
    nargs = 0;
    havenames = FALSE;
    for(pa = args ; pa != nullptr; pa = CDR(pa)) {
	if (TAG(pa) != nullptr) havenames = TRUE;
	nargs++;
    }

    PROTECT(ans = Rf_allocVector(VECSXP, nargs));
    if (havenames) {
	SEXP names;
	PROTECT(names = Rf_allocVector(STRSXP, nargs));
	for (na = 0, pa = args ; pa != nullptr ; pa = CDR(pa), na++) {
	    if (TAG(pa) == nullptr)
		SET_STRING_ELT(names, na, R_BlankString);
	    else
		SET_STRING_ELT(names, na, PRINTNAME(TAG(pa)));
	}
	Rf_setAttrib(ans, Symbols::NamesSymbol, names);
	UNPROTECT(1);
    }

    /* Convert the arguments for use in foreign function calls. */
    cargs = static_cast<void**>(RHO_alloc(nargs, sizeof(void*)));
    if (copy) cargs0 = static_cast<void**>(RHO_alloc(nargs, sizeof(void*)));
    for(na = 0, pa = args ; pa != nullptr; pa = CDR(pa), na++) {
	if(checkTypes &&
	   !comparePrimitiveTypes(checkTypes[na], CAR(pa))) {
	    /* We can loop over all the arguments and report all the
	       erroneous ones, but then we would also want to avoid
	       the conversions.  Also, in the future, we may just
	       attempt to coerce the value to the appropriate
	       type. */
	    Rf_errorcall(call, _("wrong type for argument %d in call to %s"),
		      na+1, symName);
	}
	int nprotect = 0;
	R_NativePrimitiveArgType targetType_ = checkTypes ? checkTypes[na] : 0;
	SEXPTYPE targetType = targetType_ == SINGLESXP
	    ? REALSXP : static_cast<SEXPTYPE>(targetType_);
	R_xlen_t n;
	s = CAR(pa);
	/* start with return value a copy of the inputs, as that is
	   what is needed for non-atomic-vector inputs */
	SET_VECTOR_ELT(ans, na, s);

	if(checkNativeType(targetType, TYPEOF(s)) == FALSE) {
	    /* Cannot be called if DUP = FALSE, so only needs to live
	       until copied in the switch.
	       But R_alloc allocates, so missed protection < R 2.15.0.
	    */
	    PROTECT(s = Rf_coerceVector(s, targetType));
	    nprotect++;
	}

	/* We create any copies needed for the return value here,
	   except for character vectors.  The compiled code works on
	   the data pointer of the return value for the other atomic
	   vectors, and anything else is supposed to be read-only.

	   We do not need to copy if the inputs have no references */

#ifdef LONG_VECTOR_SUPPORT
	if (Rf_isVector(s) && IS_LONG_VEC(s))
	    Rf_error(_("long vectors (argument %d) are not supported in %s"),
		  na + 1, Fort ? ".Fortran" : ".C");
#endif
	SEXPTYPE t = TYPEOF(s);
	switch(t) {
	case RAWSXP:
	    if (copy) {
		n = XLENGTH(s);
		char *ptr = R_alloc(n * sizeof(Rbyte) + 2 * NG, 1);
		memset(ptr, FILL, n * sizeof(Rbyte) + 2 * NG);
		ptr += NG;
		memcpy(ptr, RAW(s), n);
		cargs[na] = (void *) ptr;
	    } else if (MAYBE_REFERENCED(s)) {
		n = XLENGTH(s);
		SEXP ss = Rf_allocVector(t, n);
		memcpy(RAW(ss), RAW(s), n * sizeof(Rbyte));
		SET_VECTOR_ELT(ans, na, ss);
		cargs[na] = RAW(ss);
#ifdef R_MEMORY_PROFILING
		if (RTRACE(s)) memtrace_report(s, ss);
#endif
	    } else cargs[na] = RAW(s);
	    break;
	case LGLSXP:
	case INTSXP:
	    n = XLENGTH(s);
	    {
		int *iptr = INTEGER(s);
		if (!naok)
		    for (R_xlen_t i = 0 ; i < n ; i++)
			if(iptr[i] == R_NaInt)
			    Rf_error(_("NAs in foreign function call (arg %d)"), na + 1);
		if (copy) {
		    char *ptr = R_alloc(n * sizeof(int) + 2 * NG, 1);
		    memset(ptr, FILL, n * sizeof(int) + 2 * NG);
		    ptr += NG;
		    memcpy(ptr, INTEGER(s), n * sizeof(int));
		    cargs[na] = ptr;
		} else if (MAYBE_REFERENCED(s)) {
		    SEXP ss = Rf_allocVector(t, n);
		    memcpy(INTEGER(ss), INTEGER(s), n * sizeof(int));
		    SET_VECTOR_ELT(ans, na, ss);
		    cargs[na] = INTEGER(ss);
#ifdef R_MEMORY_PROFILING
		    if (RTRACE(s)) memtrace_report(s, ss);
#endif
		} else cargs[na] = iptr;
	    }
	    break;
	case REALSXP:
	    {
		n = XLENGTH(s);
		double *rptr = REAL(s);
		if (!naok)
		    for (R_xlen_t i = 0 ; i < n ; i++)
			if(!std::isfinite(rptr[i]))
			    Rf_error(_("NA/NaN/Inf in foreign function call (arg %d)"), na + 1);
		if (Rf_asLogical(Rf_getAttrib(s, CSingSymbol)) == 1) {
		    float *sptr = static_cast<float*>(RHO_alloc(n, sizeof(float)));
		    for (R_xlen_t i = 0 ; i < n ; i++) sptr[i] = float( REAL(s)[i]);
		    cargs[na] = sptr;
#ifdef R_MEMORY_PROFILING
		    if (RTRACE(s)) memtrace_report(s, sptr);
#endif
		} else if (copy) {
		    char *ptr = R_alloc(n * sizeof(double) + 2 * NG, 1);
		    memset(ptr, FILL, n * sizeof(double) + 2 * NG);
		    ptr += NG;
		    memcpy(ptr, REAL(s), n * sizeof(double));
		    cargs[na] = ptr;
		} else if (MAYBE_REFERENCED(s)) {
		    SEXP ss  = Rf_allocVector(t, n);
		    memcpy(REAL(ss), REAL(s), n * sizeof(double));
		    SET_VECTOR_ELT(ans, na, ss);
		    cargs[na] = REAL(ss);
#ifdef R_MEMORY_PROFILING
		    if (RTRACE(s)) memtrace_report(s, ss);
#endif
		} else cargs[na] = rptr;
	    }
	    break;
	case CPLXSXP:
	    {
		n = XLENGTH(s);
		Rcomplex *zptr = COMPLEX(s);
		if (!naok)
		    for (R_xlen_t i = 0 ; i < n ; i++)
			if(!std::isfinite(zptr[i].r) || !std::isfinite(zptr[i].i))
			    Rf_error(_("complex NA/NaN/Inf in foreign function call (arg %d)"), na + 1);
		if (copy) {
		    char *ptr = R_alloc(n * sizeof(Rcomplex) + 2 * NG, 1);
		    memset(ptr, FILL, n * sizeof(Rcomplex) + 2 * NG);
		    ptr += NG;
		    memcpy(ptr, COMPLEX(s), n * sizeof(Rcomplex));
		    cargs[na] = ptr;
		} else if (MAYBE_REFERENCED(s)) {
		    SEXP ss = Rf_allocVector(t, n);
		    memcpy(COMPLEX(ss), COMPLEX(s), n * sizeof(Rcomplex));
		    SET_VECTOR_ELT(ans, na, ss);
		    cargs[na] = COMPLEX(ss);
#ifdef R_MEMORY_PROFILING
		    if (RTRACE(s)) memtrace_report(s, ss);
#endif
		} else cargs[na] = zptr;
	    }
	    break;
	case STRSXP:
	    n = XLENGTH(s);
	    if (Fort) {
		const char *ss = Rf_translateChar(STRING_ELT(s, 0));
		if (n > 1)
		    Rf_warning(_("only first string in char vector used in .Fortran"));
		char *fptr = static_cast<char*>(RHO_alloc(max(size_t(255), strlen(ss)) + 1, sizeof(char)));
		strcpy(fptr, ss);
		cargs[na] =  fptr;
	    } else if (copy) {
		char **cptr = static_cast<char**>(RHO_alloc(n, sizeof(char*))),
		    **cptr0 = static_cast<char**>(RHO_alloc(n, sizeof(char*)));
		for (R_xlen_t i = 0 ; i < n ; i++) {
		    const char *ss = Rf_translateChar(STRING_ELT(s, i));
		    size_t nn = strlen(ss) + 1 + 2 * NG;
		    char *ptr = static_cast<char*>(RHO_alloc(nn, sizeof(char)));
		    memset(ptr, FILL, nn);
		    cptr[i] = cptr0[i] = ptr + NG;
		    strcpy(cptr[i], ss);
		}
		cargs[na] = cptr;
		cargs0[na] = cptr0;
#ifdef R_MEMORY_PROFILING
		if (RTRACE(s)) memtrace_report(s, cargs[na]);
#endif
	    } else {
		char **cptr = static_cast<char**>(RHO_alloc(n, sizeof(char*)));
		for (R_xlen_t i = 0 ; i < n ; i++) {
		    const char *ss = Rf_translateChar(STRING_ELT(s, i));
		    size_t nn = strlen(ss) + 1;
		    if(nn > 1) {
			cptr[i] = static_cast<char*>(RHO_alloc(nn, sizeof(char)));
			strcpy(cptr[i], ss);
		    } else {
			/* Protect ourselves against those who like to
			   extend "", maybe using strncpy */
			nn = 128;
			cptr[i] = static_cast<char*>(RHO_alloc(nn, sizeof(char)));
			memset(cptr[i], 0, nn);
		    }
		    cargs[na] = cptr;
#ifdef R_MEMORY_PROFILING
		    if (RTRACE(s)) memtrace_report(s, cargs[na]);
#endif
		}
	    }
	    break;
	case VECSXP:
	    {
		if (Fort) Rf_error(_("invalid mode (%s) to pass to Fortran (arg %d)"),
				Rf_type2char(t), na + 1);
		/* Used read-only, so this is safe */
		n = XLENGTH(s);
		SEXP *lptr = static_cast<SEXP *>(RHO_alloc(n, sizeof(SEXP)));
		for (R_xlen_t i = 0 ; i < n ; i++) lptr[i] = VECTOR_ELT(s, i);
		cargs[na] = lptr;
	    }
	    break;
	case CLOSXP:
	case BUILTINSXP:
	case SPECIALSXP:
	case ENVSXP:
	    if (Fort) Rf_error(_("invalid mode (%s) to pass to Fortran (arg %d)"),
			    Rf_type2char(t), na + 1);
	    cargs[na] =  s;
	    break;
	case NILSXP:
	    Rf_error(_("invalid mode (%s) to pass to C or Fortran (arg %d)"),
		  Rf_type2char(t), na + 1);
	    cargs[na] =  s;
	    break;
	default:
	    /* Includes pairlists from R 2.15.0 */
	    if (Fort) Rf_error(_("invalid mode (%s) to pass to Fortran (arg %d)"),
			    Rf_type2char(t), na + 1);
	    Rf_warning("passing an object of type '%s' to .C (arg %d) is deprecated",
		    Rf_type2char(t), na + 1);
	    if (t == LISTSXP)
		Rf_warning(_("pairlists are passed as SEXP as from R 2.15.0"));
	    cargs[na] =  (void*) s;
	    break;
	}
	if (nprotect) UNPROTECT(nprotect);
    }

    switch (nargs) {
    case 0:
	/* Silicon graphics C chokes here */
	/* if there is no argument to fun. */
	fun(0);
	break;
/*  This macro expands out to:
    case 1:
	fun(cargs[0]);
	break;
    case 2:
	fun(cargs[0], cargs[1]);
	break;
    ... on to case 65
*/

#define ARGUMENT_LIST(Z, N, IGNORED) BOOST_PP_COMMA_IF(N) cargs[N]
#define CASE_STATEMENT(Z, N, IGNORED)              \
    case N:                                        \
	fun(BOOST_PP_REPEAT(N, ARGUMENT_LIST, 0)); \
	break;

	BOOST_PP_REPEAT_FROM_TO(1, BOOST_PP_INC(MAX_ARGS), CASE_STATEMENT, 0);

#undef ARGUMENT_LIST
#undef CASE_STATEMENT

    default:
	Rf_errorcall(call, _("too many arguments, sorry"));
    }

    for (na = 0, pa = args ; pa != nullptr ; pa = CDR(pa), na++) {
	void *p = cargs[na];
	SEXP arg = CAR(pa);
	s = VECTOR_ELT(ans, na);
	R_NativePrimitiveArgType type_ =
	    checkTypes ? checkTypes[na] : TYPEOF(arg);
	SEXPTYPE type;
	bool single_precision;
	 if (type_ == SINGLESXP) {
		type = REALSXP;
		single_precision = true;
	 } else {
		type = static_cast<SEXPTYPE>(type_);
		single_precision = false;
	 }
	R_xlen_t n = Rf_xlength(arg);

	switch(type) {
	case RAWSXP:
	    if (copy) {
		s = Rf_allocVector(type, n);
		unsigned char *ptr = (unsigned char *) p;
		memcpy(RAW(s), ptr, n * sizeof(Rbyte));
		ptr += n * sizeof(Rbyte);
		for (int i = 0; i < NG; i++)
		    if(*ptr++ != FILL)
			Rf_error("array over-run in %s(\"%s\") in %s argument %d\n",
			      Fort ? ".Fortran" : ".C",
			      symName, Rf_type2char(type), na+1);
		ptr = (unsigned char *) p;
		for (int i = 0; i < NG; i++)
		    if(*--ptr != FILL)
			Rf_error("array under-run in %s(\"%s\") in %s argument %d\n",
			      Fort ? ".Fortran" : ".C",
			      symName, Rf_type2char(type), na+1);
	    }
	    break;
	case INTSXP:
	    if (copy) {
		s = Rf_allocVector(type, n);
		unsigned char *ptr = (unsigned char *) p;
		memcpy(INTEGER(s), ptr, n * sizeof(int));
		ptr += n * sizeof(int);
		for (int i = 0; i < NG; i++)
		    if(*ptr++ != FILL)
			Rf_error("array over-run in %s(\"%s\") in %s argument %d\n",
			      Fort ? ".Fortran" : ".C",
			      symName, Rf_type2char(type), na+1);
		ptr = (unsigned char *) p;
		for (int i = 0; i < NG; i++)
		    if(*--ptr != FILL)
			Rf_error("array under-run in %s(\"%s\") in %s argument %d\n",
			      Fort ? ".Fortran" : ".C",
			      symName, Rf_type2char(type), na+1);
	    }
	    break;
	case LGLSXP:
	    if (copy) {
		s = Rf_allocVector(type, n);
		unsigned char *ptr = (unsigned char *) p;
		int *iptr = (int*) ptr, tmp;
		for (R_xlen_t i = 0 ; i < n ; i++) {
		    tmp =  iptr[i];
		    LOGICAL(s)[i] = (tmp == R_NaInt || tmp == 0) ? tmp : 1;
		}
		ptr += n * sizeof(int);
		for (int i = 0; i < NG;  i++)
		    if(*ptr++ != FILL)
			Rf_error("array over-run in %s(\"%s\") in %s argument %d\n",
			      Fort ? ".Fortran" : ".C",
			      symName, Rf_type2char(type), na+1);
		ptr = (unsigned char *) p;
		for (int i = 0; i < NG; i++)
		    if(*--ptr != FILL)
			Rf_error("array under-run in %s(\"%s\") in %s argument %d\n",
			      Fort ? ".Fortran" : ".C",
			      symName, Rf_type2char(type), na+1);
	    } else {
		int *iptr = (int *)p, tmp;
		for (R_xlen_t i = 0 ; i < n ; i++) {
		    tmp =  iptr[i];
		    iptr[i] = (tmp == R_NaInt || tmp == 0) ? tmp : 1;
		}
	    }
	    break;
	case REALSXP:
	    if (copy) {
		s = Rf_allocVector(REALSXP, n);
		if (single_precision || Rf_asLogical(Rf_getAttrib(arg, CSingSymbol)) == 1) {
		    float *sptr = (float*) p;
		    for(R_xlen_t i = 0 ; i < n ; i++)
			REAL(s)[i] = (double) sptr[i];
		} else {
		    unsigned char *ptr = (unsigned char *) p;
		    memcpy(REAL(s), ptr, n * sizeof(double));
		    ptr += n * sizeof(double);
		    for (int i = 0; i < NG; i++)
			if(*ptr++ != FILL)
			    Rf_error("array over-run in %s(\"%s\") in %s argument %d\n",
				  Fort ? ".Fortran" : ".C",
				  symName, Rf_type2char(type), na+1);
		    ptr = (unsigned char *) p;
		    for (int i = 0; i < NG; i++)
			if(*--ptr != FILL)
			    Rf_error("array under-run in %s(\"%s\") in %s argument %d\n",
				  Fort ? ".Fortran" : ".C",
				  symName, Rf_type2char(type), na+1);
		}
	    } else {
		if (single_precision || Rf_asLogical(Rf_getAttrib(arg, CSingSymbol)) == 1) {
		    s = Rf_allocVector(REALSXP, n);
		    float *sptr = (float*) p;
		    for(int i = 0 ; i < n ; i++)
			REAL(s)[i] = (double) sptr[i];
		}
	    }
	    break;
	case CPLXSXP:
	    if (copy) {
		s = Rf_allocVector(type, n);
		unsigned char *ptr = (unsigned char *) p;
		memcpy(COMPLEX(s), p, n * sizeof(Rcomplex));
		ptr += n * sizeof(Rcomplex);
		for (int i = 0; i < NG;  i++)
		    if(*ptr++ != FILL)
			Rf_error("array over-run in %s(\"%s\") in %s argument %d\n",
			      Fort ? ".Fortran" : ".C",
			      symName, Rf_type2char(type), na+1);
		ptr = (unsigned char *) p;
		for (int i = 0; i < NG; i++)
		    if(*--ptr != FILL)
			Rf_error("array under-run in %s(\"%s\") in %s argument %d\n",
			      Fort ? ".Fortran" : ".C",
			      symName, Rf_type2char(type), na+1);
	    }
	    break;
	case STRSXP:
	    if(Fort) {
		char buf[256];
		/* only return one string: warned on the R -> Fortran step */
		strncpy(buf, static_cast<char *>(p), 255);
		buf[255] = '\0';
		PROTECT(s = Rf_allocVector(type, 1));
		SET_STRING_ELT(s, 0, Rf_mkChar(buf));
		UNPROTECT(1);
	    } else if (copy) {
		SEXP ss = arg;
		PROTECT(s = Rf_allocVector(type, n));
		char **cptr = (char**) p, **cptr0 = (char**) cargs0[na];
		for (R_xlen_t i = 0 ; i < n ; i++) {
		    unsigned char *ptr = (unsigned char *) cptr[i];
		    SET_STRING_ELT(s, i, Rf_mkChar(cptr[i]));
		    if (cptr[i] == cptr0[i]) {
			const char *z = Rf_translateChar(STRING_ELT(ss, i));
			for (int j = 0; j < NG; j++)
			    if(*--ptr != FILL)
				Rf_error("array under-run in .C(\"%s\") in character argument %d, element %d",
				      symName, na+1, (int)(i+1));
			ptr = (unsigned char *) cptr[i];
			ptr += strlen(z) + 1;
			for (int j = 0; j < NG;  j++)
			    if(*ptr++ != FILL) {
				// force termination
				unsigned char *p = ptr;
				for (int k = 1; k < NG - j; k++, p++)
				    if (*p == FILL) *p = '\0';
				Rf_error("array over-run in .C(\"%s\") in character argument %d, element %d\n'%s'->'%s'\n",
				      symName, na+1, (int)(i+1),
				      z, cptr[i]);
			    }
		    }
		}
		UNPROTECT(1);
	    } else {
		PROTECT(s = Rf_allocVector(type, n));
		char **cptr = (char**) p;
		for (R_xlen_t i = 0 ; i < n ; i++)
		    SET_STRING_ELT(s, i, Rf_mkChar(cptr[i]));
		UNPROTECT(1);
	    }
	    break;
	default:
	    break;
	}
	if (s != arg) {
	    PROTECT(s);
	    SHALLOW_DUPLICATE_ATTRIB(s, arg);
	    SET_VECTOR_ELT(ans, na, s);
	    UNPROTECT(1);
	}
    }
    UNPROTECT(1);
    vmaxset(vmax);
    return ans;
}

#ifndef NO_CALL_R
static const struct typeinfo {
    const char *name;
    const SEXPTYPE type;
}

typeinfo[] = {
    {"logical",	  LGLSXP },
    {"integer",	  INTSXP },
    {"double",	  REALSXP},
    {"complex",	  CPLXSXP},
    {"character", STRSXP },
    {"list",	  VECSXP },
    {nullptr,	  NILSXP }
};

static SEXPTYPE string2type(char *s)
{
    int i;
    for (i = 0 ; typeinfo[i].name ; i++) {
	if(streql(typeinfo[i].name, s)) {
	    return typeinfo[i].type;
	}
    }
    Rf_error(_("type \"%s\" not supported in interlanguage calls"), s);
    return NILSXP; /* for -Wall */
}

/* This is entirely legacy, with no known users (Mar 2012).
   So we freeze the code involved.
 */

static void *RObjToCPtr2(SEXP s)
{
    int n;

    switch(TYPEOF(s)) {
    case LGLSXP:
    case INTSXP:
	{
	    n = LENGTH(s);
	    int *iptr = INTEGER(s);
	    iptr = static_cast<int*>(RHO_alloc(n, sizeof(int)));
	    for (int i = 0 ; i < n ; i++) iptr[i] = INTEGER(s)[i];
	    return iptr;
	}
	break;
    case REALSXP:
	{
	    n = LENGTH(s);
	    double *rptr = REAL(s);
	    rptr = static_cast<double*>(RHO_alloc(n, sizeof(double)));
	    for (int i = 0 ; i < n ; i++) rptr[i] = REAL(s)[i];
	    return rptr;
	}
	break;
    case CPLXSXP:
	{
	    n = LENGTH(s);
	    Rcomplex *zptr = COMPLEX(s);
	    zptr = static_cast<Rcomplex*>(RHO_alloc(n, sizeof(Rcomplex)));
	    for (int i = 0 ; i < n ; i++) zptr[i] = COMPLEX(s)[i];
	    return zptr;
	}
	break;
    case STRSXP:
	{
	    n = LENGTH(s);
	    char **cptr = static_cast<char**>(RHO_alloc(n, sizeof(char*)));
	    for (int i = 0 ; i < n ; i++) {
		const char *ss = Rf_translateChar(STRING_ELT(s, i));
		cptr[i] = R_alloc(strlen(ss) + 1, sizeof(char));
		strcpy(cptr[i], ss);
	    }
	    return cptr;
	}
	break;
	/* From here down, probably not right */
    case VECSXP:
	{
	    n = Rf_length(s);
	    SEXP *lptr = static_cast<SEXP *>(RHO_alloc(n, sizeof(SEXP)));
	    for (int i = 0 ; i < n ; i++) lptr[i] = VECTOR_ELT(s, i);
	    return lptr;
	}
	break;
    default:
	return s;
    }
    return nullptr;  // -Wall
}



void call_R(char *func, long nargs, void **arguments, char **modes,
	    long *lengths, char **names, long nres, char **results)
{
    SEXP call, pcall, s;
    SEXPTYPE type;
    int i, j, n;

    if (!Rf_isFunction(reinterpret_cast<SEXP>(func)))
	Rf_error("invalid function in call_R");
    if (nargs < 0)
	Rf_error("invalid argument count in call_R");
    if (nres < 0)
	Rf_error("invalid return value count in call_R");
    GCStackRoot<PairList> tl(PairList::make(nargs));
    PROTECT(pcall = call = new Expression(nullptr, tl));
    SETCAR(pcall, reinterpret_cast<SEXP>(func));
    s = nullptr;		/* -Wall */
    for (i = 0 ; i < nargs ; i++) {
	pcall = CDR(pcall);
	type = string2type(modes[i]);
	switch(type) {
	case LGLSXP:
	case INTSXP:
	    n = int(lengths[i]);
	    SETCAR(pcall, Rf_allocVector(type, n));
	    memcpy(INTEGER(CAR(pcall)), arguments[i], n * sizeof(int));
	    break;
	case REALSXP:
	    n = int(lengths[i]);
	    SETCAR(pcall, Rf_allocVector(REALSXP, n));
	    memcpy(REAL(CAR(pcall)), arguments[i], n * sizeof(double));
	    break;
	case CPLXSXP:
	    n = int(lengths[i]);
	    SETCAR(pcall, Rf_allocVector(CPLXSXP, n));
	    memcpy(REAL(CAR(pcall)), arguments[i], n * sizeof(Rcomplex));
	    break;
	case STRSXP:
	    n = int(lengths[i]);
	    SETCAR(pcall, Rf_allocVector(STRSXP, n));
	    for (j = 0 ; j < n ; j++) {
		char *str = static_cast<char*>(arguments[i]);
		SET_STRING_ELT(CAR(pcall), i, Rf_mkChar(str));
	    }
	    break;
	default:
	    Rf_error(_("mode '%s' is not supported in call_R"), modes[i]);
	}
	if(names && names[i])
	    SET_TAG(pcall, rho::Symbol::obtain(names[i]));
	ENSURE_NAMEDMAX(CAR(pcall));
    }
    PROTECT(s = Rf_eval(call, R_GlobalEnv));
    switch(TYPEOF(s)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
	if(nres > 0)
	    results[0] = static_cast<char *>(RObjToCPtr2(s));
	break;
    case VECSXP:
	n = Rf_length(s);
	if (nres < n) n = int(nres);
	for (i = 0 ; i < n ; i++)
	    results[i] = static_cast<char *>(RObjToCPtr2(VECTOR_ELT(s, i)));
	break;
    case LISTSXP:
	n = Rf_length(s);
	if(nres < n) n = int(nres);
	for(i = 0 ; i < n ; i++) {
	    results[i] = static_cast<char *>(RObjToCPtr2(s));
	    s = CDR(s);
	}
	break;
    default:  // -Wswitch
	break;
    }
    UNPROTECT(2);
    return;
}

void call_S(char *func, long nargs, void **arguments, char **modes,
	    long *lengths, char **names, long nres, char **results)
{
    call_R(func, nargs, arguments, modes, lengths, names, nres, results);
}
#endif
