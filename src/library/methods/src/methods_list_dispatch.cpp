/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2018   The R Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
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

/* <UTF8>
   Does byte-level handling in primitive_case, but only of ASCII chars
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>

#include "RSMethods.h"
#include "localization.h"
#include "methods.h"
#include <Rinternals.h>
#define STRING_VALUE(x)		R_CHAR(Rf_asChar(x))

#if !defined(snprintf) && defined(HAVE_DECL_SNPRINTF) && !HAVE_DECL_SNPRINTF
extern int snprintf (char *s, size_t n, const char *format, ...);
#endif

/* the following utilities are included here for now, as statics.  But
   they will eventually be C implementations of slot, data.class,
   etc. */

static SEXP do_dispatch(SEXP fname, SEXP ev, SEXP mlist, int firstTry,
			int evalArgs);
static SEXP R_loadMethod(SEXP f, SEXP fname, SEXP ev);
static SEXP R_selectByPackage(SEXP f, SEXP classes, int nargs);

/* objects, mostly symbols, that are initialized once to save a little time */
static int initialized = 0;
static SEXP s_dot_Methods, s_skeleton, s_expression, s_function,
    s_getAllMethods, s_objectsEnv, s_MethodsListSelect,
    s_sys_dot_frame, s_sys_dot_call, s_sys_dot_function, s_generic,
    s_missing, s_generic_dot_skeleton, s_subset_gets, s_element_gets,
    s_argument, s_allMethods, s_base;
static SEXP R_FALSE, R_TRUE;
static Rboolean table_dispatch_on = TRUE;

/* precomputed skeletons for special primitive calls */
static SEXP R_short_skeletons, R_empty_skeletons;
static SEXP f_x_i_skeleton, fgets_x_i_skeleton, f_x_skeleton, fgets_x_skeleton;


SEXP R_quick_method_check(SEXP object, SEXP fsym, SEXP fdef);

static SEXP R_target, R_defined, R_nextMethod, R_dot_nextMethod,
    R_loadMethod_name, R_methods_name, R_tripleColon_name;

static SEXP Methods_Namespace = NULL;

static const char *check_single_string(SEXP, Rboolean, const char *);
static const char *check_symbol_or_string(SEXP obj, Rboolean nonEmpty,
                                          const char *what);
static const char *class_string(SEXP obj);

static void init_loadMethod()
{
    R_target = Rf_install("target");
    R_defined = Rf_install("defined");
    R_nextMethod = Rf_install("nextMethod");
    R_loadMethod_name = Rf_install("loadMethod");
    R_dot_nextMethod = Rf_install(".nextMethod");
    R_methods_name = Rf_install("methods");
    R_tripleColon_name = Rf_install(":::");
}


SEXP R_initMethodDispatch(SEXP envir)
{
    if(envir && !Rf_isNull(envir))
	Methods_Namespace = envir;
    if(!Methods_Namespace)
	Methods_Namespace = R_GlobalEnv;
    if(initialized)
	return(envir);

    s_dot_Methods = Rf_install(".Methods");
    s_skeleton = Rf_install("skeleton");
    s_expression = Rf_install("expression");
    s_function = Rf_install("function");
    s_getAllMethods = Rf_install("getAllMethods");
    s_objectsEnv = Rf_install("objectsEnv");
    s_MethodsListSelect = Rf_install("MethodsListSelect");
    s_sys_dot_frame = Rf_install("sys.frame");
    s_sys_dot_call = Rf_install("sys.call");
    s_sys_dot_function = Rf_install("sys.function");
    s_generic = Rf_install("generic");
    s_generic_dot_skeleton = Rf_install("generic.skeleton");
    s_subset_gets = Rf_install("[<-");
    s_element_gets = Rf_install("[[<-");
    s_argument = Rf_install("argument");
    s_allMethods = Rf_install("allMethods");

    R_FALSE = Rf_ScalarLogical(FALSE);
    R_PreserveObject(R_FALSE);
    R_TRUE = Rf_ScalarLogical(TRUE);
    R_PreserveObject(R_TRUE);

    /* some strings (NOT symbols) */
    s_missing = Rf_mkString("missing");
    Rf_setAttrib(s_missing, R_PackageSymbol, Rf_mkString("methods"));
    R_PreserveObject(s_missing);
    s_base = Rf_mkString("base");
    R_PreserveObject(s_base);
    /*  Initialize method dispatch, using the static */
    R_set_standardGeneric_ptr(
	(table_dispatch_on ? R_dispatchGeneric : R_standardGeneric)
	, Methods_Namespace);
    R_set_quick_method_check(
	(table_dispatch_on ? R_quick_dispatch : R_quick_method_check));

    /* Some special lists of primitive skeleton calls.
       These will be promises under lazy-loading.
    */
    PROTECT(R_short_skeletons =
	    Rf_findVar(Rf_install(".ShortPrimitiveSkeletons"),
		    Methods_Namespace));
    if(TYPEOF(R_short_skeletons) == PROMSXP)
	R_short_skeletons = Rf_eval(R_short_skeletons, Methods_Namespace);
    R_PreserveObject(R_short_skeletons);
    UNPROTECT(1);
    PROTECT(R_empty_skeletons =
	    Rf_findVar(Rf_install(".EmptyPrimitiveSkeletons"),
		    Methods_Namespace));
    if(TYPEOF(R_empty_skeletons) == PROMSXP)
	R_empty_skeletons = Rf_eval(R_empty_skeletons, Methods_Namespace);
    R_PreserveObject(R_empty_skeletons);
    UNPROTECT(1);
    if(R_short_skeletons == R_UnboundValue ||
       R_empty_skeletons == R_UnboundValue)
	Rf_error(_("could not find the skeleton calls for 'methods' (package detached?): expect very bad things to happen"));
    f_x_i_skeleton = VECTOR_ELT(R_short_skeletons, 0);
    fgets_x_i_skeleton = VECTOR_ELT(R_short_skeletons, 1);
    f_x_skeleton = VECTOR_ELT(R_empty_skeletons, 0);
    fgets_x_skeleton = VECTOR_ELT(R_empty_skeletons, 1);
    init_loadMethod();
    initialized = 1;
    return(envir);
}


/* simplified version of do_subset2_dflt, with no partial matching */
static SEXP R_element_named(SEXP obj, const char * what)
{
    int offset = -1, i, n;
    SEXP names = Rf_getAttrib(obj, R_NamesSymbol);
    n = Rf_length(names);
    if(n > 0) {
	for(i = 0; i < n; i++) {
	    if(streql(what, R_CHAR(STRING_ELT(names, i)))) {
		offset = i; break;
	    }
	}
    }
    if(offset < 0)
	return nullptr;
    else
	return VECTOR_ELT(obj, offset);
}

static SEXP R_insert_element(SEXP mlist, const char * what, SEXP object)
{
    SEXP sym = Rf_install(what);
    return R_subassign3_dflt(nullptr, mlist, sym, object);
}

SEXP R_el_named(SEXP object, SEXP what)
{
    const char * str;
    str = R_CHAR(Rf_asChar(what));
    return R_element_named(object, str);
}

SEXP R_set_el_named(SEXP object, SEXP what, SEXP value)
{
    const char * str;
    str = R_CHAR(Rf_asChar(what));
    return R_insert_element(object, str, value);
}

/*  */
static int n_ov = 0;

SEXP R_clear_method_selection()
{
    n_ov = 0;
    return nullptr;
}

static SEXP R_find_method(SEXP mlist, const char *klass, SEXP fname)
{
    /* find the element of the methods list that matches this class,
       but not including inheritance. */
    SEXP value, methods;
    methods = R_do_slot(mlist, s_allMethods);
    if(methods == nullptr) {
	Rf_error(_("no \"allMethods\" slot found in object of class \"%s\" used as methods list for function '%s'"),
	      class_string(mlist), R_CHAR(Rf_asChar(fname)));
	return(nullptr); /* -Wall */
    }
    value = R_element_named(methods, klass);
    return value;
}

SEXP R_quick_method_check(SEXP args, SEXP mlist, SEXP fdef)
{
    /* Match the list of (evaluated) args to the methods list. */
    SEXP object, methods, value, retValue = nullptr;
    const char *klass; int nprotect = 0;
    if(!mlist)
	return nullptr;
    methods = R_do_slot(mlist, s_allMethods);
    if(methods == nullptr)
	return nullptr;
    while(!Rf_isNull(args) && !Rf_isNull(methods)) {
	object = CAR(args); args = CDR(args);
	if(TYPEOF(object) == PROMSXP) {
	    if(PRVALUE(object) == R_UnboundValue) {
		SEXP tmp = Rf_eval(PRCODE(object), PRENV(object));
		PROTECT(tmp); nprotect++;
		SET_PRVALUE(object,  tmp);
		object = tmp;
	    }
	    else
		object = PRVALUE(object);
	}
	klass = R_CHAR(STRING_ELT(R_data_class(object, TRUE), 0));
	value = R_element_named(methods, klass);
	if(Rf_isNull(value) || Rf_isFunction(value)){
	    retValue = value;
	    break;
	}
	/* continue matching args down the tree */
	methods = R_do_slot(value, s_allMethods);
    }
    UNPROTECT(nprotect);
    return(retValue);
}

SEXP R_quick_dispatch(SEXP args, SEXP genericEnv, SEXP fdef)
{
    /* Match the list of (evaluated) args to the methods table. */
    static SEXP  R_allmtable = NULL, R_siglength;
    SEXP object, value, mtable;
    const char *klass; int nprotect = 0, nsig, nargs;
#define NBUF 200
    char buf[NBUF]; char *ptr;
    if(!R_allmtable) {
	R_allmtable = Rf_install(".AllMTable");
	R_siglength = Rf_install(".SigLength");
    }
    if(!genericEnv || TYPEOF(genericEnv) != ENVSXP)
	return nullptr; /* a bug or not initialized yet */
    mtable = Rf_findVarInFrame(genericEnv, R_allmtable);
    if(mtable == R_UnboundValue || TYPEOF(mtable) != ENVSXP)
	return nullptr;
    PROTECT(mtable);
    object = Rf_findVarInFrame(genericEnv, R_siglength);
    if(object == R_UnboundValue) {
	UNPROTECT(1); /* mtable */
	return nullptr;
    }
    switch(TYPEOF(object)) {
    case REALSXP:
	if(LENGTH(object) > 0)
	    nsig = (int) REAL(object)[0];
	else {
	    UNPROTECT(1); /* mtable */
	    return nullptr;
	}
	break;
    case INTSXP:
	if(LENGTH(object) > 0)
	    nsig = (int) INTEGER(object)[0];
	else {
	    UNPROTECT(1); /* mtable */
	    return nullptr;
	}
	break;
    default:
	UNPROTECT(1); /* mtable */
	return nullptr;
    }
    buf[0] = '\0'; ptr = buf;
    nargs = 0;
    nprotect = 1; /* mtable */
    while(!Rf_isNull(args) && nargs < nsig) {
	object = CAR(args); args = CDR(args);
	if(TYPEOF(object) == PROMSXP) {
	    if(PRVALUE(object) == R_UnboundValue) {
		SEXP tmp = Rf_eval(PRCODE(object), PRENV(object));
		PROTECT(tmp); nprotect++;
		SET_PRVALUE(object,  tmp);
		object = tmp;
	    }
	    else
		object = PRVALUE(object);
	}
	if(object == R_MissingArg)
	    klass = "missing";
	else
	    klass = R_CHAR(STRING_ELT(R_data_class(object, TRUE), 0));
	if(ptr - buf + strlen(klass) + 2 > NBUF) {
	    UNPROTECT(nprotect);
	    return nullptr;
	}
	/* NB:  this code replicates .SigLabel().
	   If that changes, e.g. to include
	   the package, the code here must change too.
	   Or, better, the two should use the same C code. */
	if(ptr > buf) { ptr = strcpy(ptr, "#");  ptr += 1;}
	ptr = strcpy(ptr, klass); ptr += strlen(klass);
	nargs++;
    }
    for(; nargs < nsig; nargs++) {
	if(ptr - buf + strlen("missing") + 2 > NBUF) {
	    UNPROTECT(nprotect);
	    return nullptr;
	}
	ptr = strcpy(ptr, "#"); ptr +=1;
	ptr = strcpy(ptr, "missing"); ptr += strlen("missing");
    }
    value = Rf_findVarInFrame(mtable, Rf_install(buf));
    if(value == R_UnboundValue)
	value = nullptr;
    UNPROTECT(nprotect);
    return(value);
}

/* call some S language functions */

static SEXP R_S_MethodsListSelect(SEXP fname, SEXP ev, SEXP mlist, SEXP f_env)
{
    SEXP e, val; int n, check_err;
    n = Rf_isNull(f_env) ? 4 : 5;
    PROTECT(e = Rf_allocVector(LANGSXP, n));
    SETCAR(e, s_MethodsListSelect);
    val = CDR(e);
    SETCAR(val, fname);
    val = CDR(val);
    SETCAR(val, ev);
    val = CDR(val);
    SETCAR(val, mlist);
    if(n == 5) {
	    val = CDR(val);
	    SETCAR(val, f_env);
    }
    val = R_tryEvalSilent(e, Methods_Namespace, &check_err);
    if(check_err)
	Rf_error("S language method selection got an error when called from internal dispatch for function '%s'",
	      check_symbol_or_string(fname, TRUE,
				     "Function name for method selection called internally"));
    UNPROTECT(1);
    return val;
}


/* quick tests for generic and non-generic functions.  May mistakenly
   identify non-generics as generics:  a class with data part of type
   CLOSXP and with a slot/attribute named "generic" will qualify.
*/
#define IS_NON_GENERIC(vl) (TYPEOF(vl) == BUILTINSXP ||TYPEOF(vl) == SPECIALSXP || \
            (TYPEOF(vl) == CLOSXP && Rf_getAttrib(vl, s_generic) == nullptr))
#define IS_GENERIC(vl) (TYPEOF(vl) == CLOSXP && Rf_getAttrib(vl, s_generic) != nullptr)
#define PACKAGE_SLOT(vl) Rf_getAttrib(vl, R_PackageSymbol)

static SEXP get_generic(SEXP symbol, SEXP rho, SEXP package)
{
    SEXP vl, generic = R_UnboundValue, gpackage; const char *pkg; Rboolean ok;
    if(!Rf_isSymbol(symbol))
	symbol = Rf_installTrChar(Rf_asChar(symbol));
    pkg = R_CHAR(STRING_ELT(package, 0)); /* package is guaranteed single string */

    while (rho != nullptr) {
	vl = Rf_findVarInFrame(rho, symbol);
	if (vl != R_UnboundValue) {
	    if (TYPEOF(vl) == PROMSXP) {
		PROTECT(vl);
		vl = Rf_eval(vl, rho);
		UNPROTECT(1);
	    }
	    ok = FALSE;
	    if(IS_GENERIC(vl)) {
	      if(strlen(pkg)) {
		  gpackage = PACKAGE_SLOT(vl);
		  check_single_string(gpackage, FALSE, "The \"package\" slot in generic function object");
		  ok = streql(pkg, R_CHAR(STRING_ELT(gpackage, 0)));
		}
		else
		  ok = TRUE;
	    }
	    if(ok) {
		generic = vl;
		break;
	    } else
		vl = R_UnboundValue;
	}
	rho = ENCLOS(rho);
    }
    /* look in base if either generic is missing */
    if(generic == R_UnboundValue) {
	vl = SYMVALUE(symbol);
	if(IS_GENERIC(vl)) {
	    generic = vl;
	    if(strlen(pkg)) {
		gpackage = PACKAGE_SLOT(vl);
		check_single_string(gpackage, FALSE, "The \"package\" slot in generic function object");
		if(strcmp(pkg, R_CHAR(STRING_ELT(gpackage, 0)))) generic = R_UnboundValue;
	    }
	}
    }
    return generic;
}

SEXP R_getGeneric(SEXP name, SEXP mustFind, SEXP env, SEXP package)
{
    SEXP value;
    if(Rf_isSymbol(name)) {}
    else check_single_string(name, TRUE, "The argument \"f\" to getGeneric");
    check_single_string(package, FALSE, "The argument \"package\" to getGeneric");
    value = get_generic(name, env, package);
    if(value == R_UnboundValue) {
	if(Rf_asLogical(mustFind)) {
	    if(env == R_GlobalEnv)
		Rf_error(_("no generic function definition found for '%s'"),
		  R_CHAR(Rf_asChar(name)));
	    else
		Rf_error(_("no generic function definition found for '%s' in the supplied environment"),
		  R_CHAR(Rf_asChar(name)));
	}
	value = nullptr;
    }
    return value;
}


/* C version of the standardGeneric R function. */
SEXP R_standardGeneric(SEXP fname, SEXP ev, SEXP fdef)
{
    SEXP f_env=R_BaseEnv, mlist=nullptr, f, val=nullptr, fsym; /* -Wall */
    int nprotect = 0;

    if(!initialized)
	R_initMethodDispatch(NULL);
    fsym = fname;
    /* TODO:  the code for do_standardGeneric does a test of fsym,
     * with a less informative error message.  Should combine them.*/
    if(!Rf_isSymbol(fsym)) {
	const char *fname = check_single_string(fsym, TRUE, "The function name in the call to standardGeneric");
	fsym = Rf_install(fname);
    }
    switch(TYPEOF(fdef)) {
    case CLOSXP:
        f_env = CLOENV(fdef);
	PROTECT(mlist = Rf_findVar(s_dot_Methods, f_env)); nprotect++;
	if(mlist == R_UnboundValue)
            mlist = nullptr;
	break;
    case SPECIALSXP: case BUILTINSXP:
        f_env = R_BaseEnv;
	PROTECT(mlist = R_primitive_methods(fdef)); nprotect++;
	break;
    default: Rf_error(_("invalid generic function object for method selection for function '%s': expected a function or a primitive, got an object of class \"%s\""),
		   R_CHAR(Rf_asChar(fsym)), class_string(fdef));
    }
    switch(TYPEOF(mlist)) {
    case NILSXP:
    case CLOSXP:
    case SPECIALSXP: case BUILTINSXP:
	f = mlist; break;
    default:
	f = do_dispatch(fname, ev, mlist, TRUE, TRUE);
    }
    if(Rf_isNull(f)) {
	SEXP value;
	PROTECT(value = R_S_MethodsListSelect(fname, ev, mlist, f_env)); nprotect++;
	if(Rf_isNull(value))
	    Rf_error(_("no direct or inherited method for function '%s' for this call"),
		  R_CHAR(Rf_asChar(fname)));
	mlist = value;
	/* now look again.  This time the necessary method should
	   have been inserted in the MethodsList object */
	f = do_dispatch(fname, ev, mlist, FALSE, TRUE);
    }
    /* loadMethod methods */
    if(Rf_isObject(f))
	f = R_loadMethod(f, fname, ev);
    switch(TYPEOF(f)) {
    case CLOSXP:
	{
	    if (Rf_inherits(f, "internalDispatchMethod")) {
                val = R_deferred_default_method();
            } else {
                PROTECT(f); nprotect++; /* is this needed?? */
                val = R_execMethod(f, ev);
            }
	}
	break;
    case SPECIALSXP: case BUILTINSXP:
	/* primitives  can't be methods; they arise only as the
	   default method when a primitive is made generic.  In this
	   case, return a special marker telling the C code to go on
	   with the internal computations. */
      val = R_deferred_default_method();
      break;
    default:
	Rf_error(_("invalid object (non-function) used as method"));
	break;
    }
    UNPROTECT(nprotect);
    return val;
}

/* Is the argument missing?  This _approximates_ the classic S sense of
   the question (is the argument missing in the call), rather than the
   R semantics (is the value of the argument R_MissingArg), but not if
   computations in the body of the function may have assigned to the
   argument name.
*/
static Rboolean is_missing_arg(SEXP symbol, SEXP ev)
{
    R_varloc_t loc;

    /* Sanity check, so don't translate */
    if (!Rf_isSymbol(symbol)) Rf_error("'symbol' must be a SYMSXP");
    loc = R_findVarLocInFrame(ev, symbol);
    if (loc == NULL)
	Rf_error(_("could not find symbol '%s' in frame of call"),
	      R_CHAR(PRINTNAME(symbol)));
    return R_GetVarLocMISSING(loc);
}

SEXP R_missingArg(SEXP symbol, SEXP ev)
{
    if(!Rf_isSymbol(symbol))
	Rf_error(_("invalid symbol in checking for missing argument in method dispatch: expected a name, got an object of class \"%s\""),
	     class_string(symbol));
    if (Rf_isNull(ev)) {
	Rf_error(_("use of NULL environment is defunct"));
	ev = R_BaseEnv;
    } else
    if(!Rf_isEnvironment(ev))
	Rf_error(_("invalid environment in checking for missing argument, '%s', in methods dispatch: got an object of class \"%s\""),
	     R_CHAR(PRINTNAME(symbol)), class_string(ev));
    if(is_missing_arg(symbol, ev))
	return R_TRUE;
    else
	return R_FALSE;
}



SEXP R_selectMethod(SEXP fname, SEXP ev, SEXP mlist, SEXP evalArgs)
{
    return do_dispatch(fname, ev, mlist, TRUE, Rf_asLogical(evalArgs));
}

static SEXP do_dispatch(SEXP fname, SEXP ev, SEXP mlist, int firstTry,
			int evalArgs)
{
    const char *klass;
    SEXP arg_slot, arg_sym, method, value = nullptr;
    int nprotect = 0;
    /* check for dispatch turned off inside MethodsListSelect */
    if(Rf_isFunction(mlist))
	return mlist;
    PROTECT(arg_slot = R_do_slot(mlist, s_argument)); nprotect++;
    if(arg_slot == nullptr) {
	Rf_error(_("object of class \"%s\" used as methods list for function '%s' ( no 'argument' slot)"),
	      class_string(mlist), R_CHAR(Rf_asChar(fname)));
	return(nullptr); /* -Wall */
    }
    if(TYPEOF(arg_slot) == SYMSXP)
	arg_sym = arg_slot;
    else
	/* shouldn't happen, since argument in class MethodsList has class
	   "name" */
	arg_sym = Rf_installTrChar(Rf_asChar(arg_slot));
    if(arg_sym == R_DotsSymbol || DDVAL(arg_sym) > 0)
	Rf_error(_("(in selecting a method for function '%s') '...' and related variables cannot be used for methods dispatch"),
	      R_CHAR(Rf_asChar(fname)));
    if(TYPEOF(ev) != ENVSXP) {
	Rf_error(_("(in selecting a method for function '%s') the 'environment' argument for dispatch must be an R environment; got an object of class \"%s\""),
	    R_CHAR(Rf_asChar(fname)), class_string(ev));
	return(nullptr); /* -Wall */
    }
    /* find the symbol in the frame, but don't use eval, yet, because
       missing arguments are ok & don't require defaults */
    if(evalArgs) {
	if(is_missing_arg(arg_sym, ev))
	    klass = "missing";
	else {
	    /*  get its class */
	    SEXP arg, class_obj; int check_err;
	    PROTECT(arg = R_tryEvalSilent(arg_sym, ev, &check_err)); nprotect++;
	    if(check_err)
		Rf_error(_("error in evaluating the argument '%s' in selecting a method for function '%s': %s"),
		      R_CHAR(PRINTNAME(arg_sym)),R_CHAR(Rf_asChar(fname)),
		      R_curErrorBuf());
	    PROTECT(class_obj = R_data_class(arg, TRUE)); nprotect++;
	    klass = R_CHAR(STRING_ELT(class_obj, 0));
	}
    }
    else {
	/* the arg contains the class as a string */
	SEXP arg; int check_err;
	PROTECT(arg = R_tryEvalSilent(arg_sym, ev, &check_err)); nprotect++;
	if(check_err)
	    Rf_error(_("error in evaluating the argument '%s' in selecting a method for function '%s': %s"),
		  R_CHAR(PRINTNAME(arg_sym)),R_CHAR(Rf_asChar(fname)),
		  R_curErrorBuf());
	klass = R_CHAR(Rf_asChar(arg));
    }
    method = R_find_method(mlist, klass, fname);
    if(Rf_isNull(method)) {
      if(!firstTry)
	Rf_error(_("no matching method for function '%s' (argument '%s', with class \"%s\")"),
	      Rf_EncodeChar(Rf_asChar(fname)), Rf_EncodeChar(PRINTNAME(arg_sym)), klass);
      UNPROTECT(nprotect);
      return(nullptr);
    }
    if(value == R_MissingArg) {/* the check put in before calling
			  function  MethodListSelect in R */
      Rf_error(_("recursive use of function '%s' in method selection, with no default method"),
		  R_CHAR(Rf_asChar(fname)));
      return(nullptr);
    }
    if(!Rf_isFunction(method)) {
	/* assumes method is a methods list itself.  */
	/* call do_dispatch recursively.  Note the NULL for fname; this is
	   passed on to the S language search function for inherited
	   methods, to indicate a recursive call, not one to be stored in
	   the methods metadata */
	method = do_dispatch(nullptr, ev, method, firstTry, evalArgs);
    }
    UNPROTECT(nprotect); nprotect = 0;
    return method;
}

SEXP R_M_setPrimitiveMethods(SEXP fname, SEXP op, SEXP code_vec,
			     SEXP fundef, SEXP mlist)
{
    return R_set_prim_method(fname, op, code_vec, fundef, mlist);
    // -> ../../../main/objects.cpp
}

SEXP R_nextMethodCall(SEXP matched_call, SEXP ev)
{
    SEXP e, val, args, this_sym, op;
    int i, nargs = Rf_length(matched_call)-1, error_flag;
    Rboolean prim_case;
    /* for primitive .nextMethod's, suppress further dispatch to avoid
     * going into an infinite loop of method calls
    */
    PROTECT(op = Rf_findVarInFrame3(ev, R_dot_nextMethod, TRUE));
    if(op == R_UnboundValue)
	Rf_error("internal error in 'callNextMethod': '.nextMethod' was not assigned in the frame of the method call");
    PROTECT(e = Rf_duplicate(matched_call));
    prim_case = Rf_isPrimitive(op);
    if (!prim_case) {
        if (Rf_inherits(op, "internalDispatchMethod")) {
	    SEXP generic = Rf_findVarInFrame3(ev, rho::Symbols::DotGenericSymbol, TRUE);
	    if(generic == R_UnboundValue)
	        Rf_error("internal error in 'callNextMethod': '.Generic' was not assigned in the frame of the method call");
	    PROTECT(generic);
	    op = INTERNAL(Rf_installTrChar(Rf_asChar(generic)));
	    UNPROTECT(1); /* generic */
	    prim_case = TRUE;
	}
    }
    if(prim_case) {
	/* retain call to primitive function, suppress method
	   dispatch for it */
        do_set_prim_method(op, "suppress", nullptr, nullptr);
    }
    else
	SETCAR(e, R_dot_nextMethod); /* call .nextMethod instead */
    args = CDR(e);
    /* e is a copy of a match.call, with expand.dots=FALSE.  Turn each
    <TAG>=value into <TAG> = <TAG>, except  ...= is skipped (if it
    appears) in which case ... was appended. */
    for(i=0; i<nargs; i++) {
	this_sym = TAG(args);
  /* "missing" only possible in primitive */
        if(this_sym != nullptr && CAR(args) != R_MissingArg)
	    SETCAR(args, this_sym);
	args = CDR(args);
    }
    if(prim_case) {
	val = R_tryEvalSilent(e, ev, &error_flag);
	/* reset the methods:  nullptr for the mlist argument
	   leaves the previous function, methods list unchanged */
	do_set_prim_method(op, "set", nullptr, nullptr);
	if(error_flag)
	    Rf_error(_("error in evaluating a 'primitive' next method: %s"),
		     R_curErrorBuf());
    }
    else
	val = Rf_eval(e, ev);
    UNPROTECT(2);
    return val;
}


static SEXP R_loadMethod(SEXP def, SEXP fname, SEXP ev)
{
    /* since this is called every time a method is dispatched with a
       definition that has a class, it should be as efficient as
       possible => we build in knowledge of the standard
       MethodDefinition and MethodWithNext slots.  If these (+ the
       class slot) don't account for all the attributes, regular
       dispatch is done. */
    SEXP s, attrib;
    int found = 1; /* we "know" the class attribute is there */
    PROTECT(def);
    for(s = attrib = ATTRIB(def); s != nullptr; s = CDR(s)) {
	SEXP t = TAG(s);
	if(t == R_target) {
	    Rf_defineVar(rho::Symbols::DottargetSymbol, CAR(s), ev); found++;
	}
	else if(t == R_defined) {
	    Rf_defineVar(rho::Symbols::DotdefinedSymbol, CAR(s), ev); found++;
	}
	else if(t == R_nextMethod)  {
	    Rf_defineVar(R_dot_nextMethod, CAR(s), ev); found++;
	}
	else if(t == R_SourceSymbol || t == s_generic)  {
	    /* ignore */ found++;
	}
    }
    Rf_defineVar(rho::Symbols::DotMethodSymbol, def, ev);

    if(found < Rf_length(attrib)) {
        /* this shouldn't be needed but check the generic being
           "loadMethod", which would produce a recursive loop */
        if(streql(R_CHAR(Rf_asChar(fname)), "loadMethod")) {
	    UNPROTECT(1);
            return def;
	}
	SEXP e, val;
	PROTECT(e = Rf_allocVector(LANGSXP, 4));
	SETCAR(e,
	       Rf_lang3(R_tripleColon_name, R_methods_name, R_loadMethod_name));
	val = CDR(e);
	SETCAR(val, def); val = CDR(val);
	SETCAR(val, fname); val = CDR(val);
	SETCAR(val, ev);
	val = Rf_eval(e, ev);
	UNPROTECT(2);
	return val;
    }
    else {
	UNPROTECT(1);
	return def;
    }
}

static SEXP R_selectByPackage(SEXP table, SEXP classes, int nargs) {
    int lwidth, i; SEXP thisPkg;
    char *buf, *bufptr;
    lwidth = 0;
    for(i = 0; i<nargs; i++) {
	thisPkg = PACKAGE_SLOT(VECTOR_ELT(classes, i));
	if(thisPkg == nullptr)
	    thisPkg = s_base;
	lwidth += (int) strlen(STRING_VALUE(thisPkg)) + 1;
    }
    /* make the label */
    const void *vmax = vmaxget();
    buf = (char *) R_alloc(lwidth + 1, sizeof(char));
    bufptr = buf;
    for(i = 0; i<nargs; i++) {
	if(i > 0)
	    *bufptr++ = '#';
	thisPkg = PACKAGE_SLOT(VECTOR_ELT(classes, i));
	if(thisPkg == nullptr)
	    thisPkg = s_base;
	strcpy(bufptr, STRING_VALUE(thisPkg));
	while(*bufptr)
	    bufptr++;
    }
    /* look up the method by package -- if R_unboundValue, will go on
     to do inherited calculation */
    SEXP sym = Rf_install(buf);
    vmaxset(vmax);
    return Rf_findVarInFrame(table, sym);
}

static const char *
check_single_string(SEXP obj, Rboolean nonEmpty, const char *what)
{
    const char *string_ = "<unset>"; /* -Wall */
    if(Rf_isString(obj)) {
	if(Rf_length(obj) != 1)
	    Rf_error(_("'%s' must be a single string (got a character vector of length %d)"),
		  what, Rf_length(obj));
	string_ = R_CHAR(STRING_ELT(obj, 0));
	if(nonEmpty && (! string_ || !string_[0]))
	    Rf_error(_("'%s' must be a non-empty string; got an empty string"),
		  what);
    }
    else {
	Rf_error(_("'%s' must be a single string (got an object of class \"%s\")"),
	      what, class_string(obj));
    }
    return string_;
}

static const char *check_symbol_or_string(SEXP obj, Rboolean nonEmpty,
                                          const char *what)
{
    if(Rf_isSymbol(obj))
	return R_CHAR(PRINTNAME(obj));
    else
	return check_single_string(obj, nonEmpty, what);
}

static const char *class_string(SEXP obj)
{
    return R_CHAR(STRING_ELT(R_data_class(obj, TRUE), 0));
}

/* internal version of paste(".", prefix, name, sep="__"),
   for speed so few checks

   If you decide to change this:
   - don't, you will break all installed S4-using packages!
   - change the hard-coded ".__M__" in namespace.R
*/
SEXP R_methodsPackageMetaName(SEXP prefix, SEXP name, SEXP pkg)
{
    char str[501];
    const char *prefixString, *nameString, *pkgString;

    prefixString = check_single_string(prefix, TRUE,
				       "The internal prefix (e.g., \"C\") for a meta-data object");
    nameString = check_single_string(name, FALSE,
				     "The name of the object (e.g,. a class or generic function) to find in the meta-data");
    pkgString = check_single_string(pkg, FALSE,
				   "The name of the package for a meta-data object");
    if(*pkgString)
      snprintf(str, 500, ".__%s__%s:%s", prefixString, nameString, pkgString);
    else
      snprintf(str, 500, ".__%s__%s", prefixString, nameString);
    return Rf_mkString(str);
}

SEXP R_identC(SEXP e1, SEXP e2)
{
    if(TYPEOF(e1) == STRSXP && TYPEOF(e2) == STRSXP &&
       LENGTH(e1) == 1 && LENGTH(e2) == 1 &&
       STRING_ELT(e1, 0) == STRING_ELT(e2, 0))
	return R_TRUE;
    else
	return R_FALSE;
}

SEXP R_getClassFromCache(SEXP klass, SEXP table)
{
    SEXP value;
    if(TYPEOF(klass) == STRSXP) {
	if (LENGTH(klass) == 0) return nullptr;
	SEXP package = PACKAGE_SLOT(klass);
	value = Rf_findVarInFrame(table, Rf_installTrChar(STRING_ELT(klass, 0)));
	if(value == R_UnboundValue)
	    return nullptr;
	else if(TYPEOF(package) == STRSXP) {
	    SEXP defPkg = PACKAGE_SLOT(value);
	    /* check equality of package */
	    if(TYPEOF(defPkg) == STRSXP && Rf_length(defPkg) ==1 &&
	       STRING_ELT(defPkg,0) != STRING_ELT(package, 0))
		return nullptr;
	    else
		return value;
	}
	else /* may return a list if multiple instances of class */
	    return value;
    }
    else if(TYPEOF(klass) != S4SXP) {
	Rf_error(_("class should be either a character-string name or a class definition"));
	return nullptr; /* NOT REACHED */
    } else /* assumes a class def, but might check */
	return klass;
}


static SEXP do_inherited_table(SEXP class_objs, SEXP fdef, SEXP mtable, SEXP ev)
{
    static SEXP dotFind = NULL, f; SEXP  e, ee;
    if(dotFind == NULL) {
	dotFind = Rf_install(".InheritForDispatch");
	f = Rf_findFun(dotFind, R_MethodsNamespace);
    }
    PROTECT(e = Rf_allocVector(LANGSXP, 4));
    SETCAR(e, f); ee = CDR(e);
    SETCAR(ee, class_objs); ee = CDR(ee);
    SETCAR(ee, fdef); ee = CDR(ee);
    SETCAR(ee, mtable);
    ee = Rf_eval(e, ev);
    UNPROTECT(1);
    return ee;
}

static SEXP dots_class(SEXP ev, int *checkerrP)
{
    static SEXP call = NULL; SEXP  ee;
    if(call == NULL) {
	SEXP dotFind, f, R_dots;
	dotFind = Rf_install(".dotsClass");
	PROTECT(f = Rf_findFun(dotFind, R_MethodsNamespace));
	R_dots = Rf_install("...");
	call = Rf_allocVector(LANGSXP, 2);
	R_PreserveObject(call);
	SETCAR(call,f); ee = CDR(call);
	SETCAR(ee, R_dots);
	UNPROTECT(1);
    }
    return R_tryEvalSilent(call, ev, checkerrP);
}

static SEXP do_mtable(SEXP fdef, SEXP ev)
{
    static SEXP dotFind = NULL, f; SEXP  e, ee;
    if(dotFind == NULL) {
	dotFind = Rf_install(".getMethodsTable");
	f = Rf_findFun(dotFind, R_MethodsNamespace);
	R_PreserveObject(f);
    }
    PROTECT(e = Rf_allocVector(LANGSXP, 2));
    SETCAR(e, f); ee = CDR(e);
    SETCAR(ee, fdef);
    ee = Rf_eval(e, ev);
    UNPROTECT(1);
    return ee;
}

SEXP R_dispatchGeneric(SEXP fname, SEXP ev, SEXP fdef)
{
    static SEXP R_mtable = NULL, R_allmtable, R_sigargs, R_siglength, R_dots;
    int nprotect = 0;
    SEXP mtable, classes, thisClass = nullptr /* -Wall */, sigargs,
	siglength, f_env = nullptr, method, f, val = nullptr;
    char *buf, *bufptr;
    int nargs, i, lwidth = 0;

    if(!R_mtable) {
	R_mtable = Rf_install(".MTable");
	R_allmtable = Rf_install(".AllMTable");
	R_sigargs = Rf_install(".SigArgs");
	R_siglength = Rf_install(".SigLength");
	R_dots = Rf_install("...");
    }
    switch(TYPEOF(fdef)) {
    case CLOSXP:
        f_env = CLOENV(fdef);
	break;
    case SPECIALSXP: case BUILTINSXP:
	PROTECT(fdef = R_primitive_generic(fdef)); nprotect++;
	if(TYPEOF(fdef) != CLOSXP) {
	    Rf_error(_("failed to get the generic for the primitive \"%s\""),
		  R_CHAR(Rf_asChar(fname)));
	    return nullptr;
	}
	f_env = CLOENV(fdef);
	break;
    default:
	Rf_error(_("expected a generic function or a primitive for dispatch, got an object of class \"%s\""),
	      class_string(fdef));
    }
    PROTECT(mtable = Rf_findVarInFrame(f_env, R_allmtable)); nprotect++;
    if(mtable == R_UnboundValue) {
	do_mtable(fdef, ev); /* Should initialize the generic */
	PROTECT(mtable = Rf_findVarInFrame(f_env, R_allmtable)); nprotect++;
    }
    PROTECT(sigargs = Rf_findVarInFrame(f_env, R_sigargs)); nprotect++;
    PROTECT(siglength = Rf_findVarInFrame(f_env, R_siglength)); nprotect++;
    if(sigargs == R_UnboundValue || siglength == R_UnboundValue ||
       mtable == R_UnboundValue)
	Rf_error("generic \"%s\" seems not to have been initialized for table dispatch---need to have '.SigArgs' and '.AllMtable' assigned in its environment");
    nargs = Rf_asInteger(siglength);
    PROTECT(classes = Rf_allocVector(VECSXP, nargs)); nprotect++;
    if (nargs > LENGTH(sigargs))
	Rf_error("'.SigArgs' is shorter than '.SigLength' says it should be");
    for(i = 0; i < nargs; i++) {
	SEXP arg_sym = VECTOR_ELT(sigargs, i);
	if(is_missing_arg(arg_sym, ev))
	    thisClass = s_missing;
	else {
	    /*  get its class */
	    SEXP arg; int check_err = 0;
	    if(arg_sym == R_dots) {
		thisClass = dots_class(ev, &check_err);
	    }
	    else {
		PROTECT(arg = Rf_eval(arg_sym, ev));
		/* PROTECT(arg = R_tryEvalSilent(arg_sym, ev, &check_err)); // <- related to bug PR#16111 */
		/* if(!check_err) */
		thisClass = R_data_class(arg, TRUE);
		UNPROTECT(1); /* arg */
	    }
	    if(check_err)
		Rf_error(_("error in evaluating the argument '%s' in selecting a method for function '%s': %s"),
		      R_CHAR(PRINTNAME(arg_sym)), R_CHAR(Rf_asChar(fname)),
		      R_curErrorBuf());
	}
	SET_VECTOR_ELT(classes, i, thisClass);
	lwidth += (int) strlen(STRING_VALUE(thisClass)) + 1;
    }
    /* make the label */
    const void *vmax = vmaxget();
    buf = (char *) R_alloc(lwidth + 1, sizeof(char));
    bufptr = buf;
    for(i = 0; i<nargs; i++) {
	if(i > 0)
	    *bufptr++ = '#';
	thisClass = VECTOR_ELT(classes, i);
	strcpy(bufptr, STRING_VALUE(thisClass));
	while(*bufptr)
	    bufptr++;
    }
    method = Rf_findVarInFrame(mtable, rho::Symbol::obtain(buf));
    vmaxset(vmax);
    if(TYPEOF(method) == ENVSXP) {
	PROTECT(method);
	method = R_selectByPackage(method, classes, nargs);
	UNPROTECT(1);
    }
    if(method == R_UnboundValue) {
	method = do_inherited_table(classes, fdef, mtable, ev);
    }
    /* the rest of this is identical to R_standardGeneric;
       hence the f=method to remind us  */
    f = method;
    switch(TYPEOF(f)) {
    case CLOSXP:
    {
        if (Rf_inherits(f, "internalDispatchMethod")) {
            val = R_deferred_default_method();
        } else {
            if(Rf_isObject(f))
                f = R_loadMethod(f, fname, ev);
            PROTECT(f); nprotect++; /* is this needed?? */
            val = R_execMethod(f, ev);
        }
    }
    break;
    case SPECIALSXP: case BUILTINSXP:
	/* primitives  can't be methods; they arise only as the
	   default method when a primitive is made generic.  In this
	   case, return a special marker telling the C code to go on
	   with the internal computations. */
	val = R_deferred_default_method();
	break;
    default:
	Rf_error(_("invalid object (non-function) used as method"));
	break;
    }
    UNPROTECT(nprotect);
    return val;
}

SEXP R_set_method_dispatch(SEXP onOff)
{
    Rboolean prev = table_dispatch_on, value = Rboolean(Rf_asLogical(onOff));
    if(value == NA_LOGICAL) /*  just return previous*/
	value = prev;
    table_dispatch_on = value;
    if(value != prev) {
	R_set_standardGeneric_ptr(
	    (table_dispatch_on ? R_dispatchGeneric : R_standardGeneric),
	    Methods_Namespace);
	R_set_quick_method_check(
	    (table_dispatch_on ? R_quick_dispatch : R_quick_method_check));
    }
    return Rf_ScalarLogical(prev);
}
