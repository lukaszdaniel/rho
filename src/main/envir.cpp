/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2017  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 *
 *  Environments:
 *
 *  All the action of associating values with symbols happens
 *  in this code.  An environment is (essentially) a list of
 *  environment "frames" of the form
 *
 *	FRAME(envir) = environment frame
 *	ENCLOS(envir) = parent environment
 *
 *  Each frame is a (tagged) list with
 *
 *	TAG(item) = symbol
 *	CAR(item) = value bound to symbol in this frame
 *	CDR(item) = next value on the list
 *
 *  When the value of a symbol is required, the environment is
 *  traversed frame-by-frame until a value is found.
 *
 *  If a value is not found during the traversal, the symbol's
 *  "value" slot is inspected for a value.  This "top-level"
 *  environment is where system functions and variables reside.
 *
 */

/* R 1.8.0: namespaces are no longer experimental, so the following
 *  are no longer 'experimental options', but rather three sections
 *  describing the API:
 *
 * NAMESPACES:
 *     R_BaseNamespace holds an environment that has R_GlobalEnv as
 *     its parent.  This environment does not actually contain any
 *     bindings of its own.  Instead, it redirects all fetches and
 *     assignments to the SYMVALUE fields of the base (R_BaseEnv)
 *     environment.  If evaluation occurs in R_BaseNamespace, then
 *     base is searched before R_GlobalEnv.
 *
 * ENVIRONMENT_LOCKING: Locking an environment prevents new bindings
 *     from being created and existing bindings from being removed.
 *
 * FANCY_BINDINGS: We have binding locking and "active bindings".
 *     When a binding is locked, its value cannot be changed.  It may
 *     still be removed from the environment if the environment is not
 *     locked.
 *
 *     Active bindings contain a function in their value cell.
 *     Getting the value of an active binding calls this function with
 *     no arguments and returns the result.  Assigning to an active
 *     binding calls this function with one argument, the new value.
 *     Active bindings may be useful for mapping external variables,
 *     such as C variables or data base entries, to R variables.  They
 *     may also be useful for making some globals thread-safe.
 *
 *     Bindings are marked as locked or active using bits 14 and 15 in
 *     their gp fields.  Since the save/load code writes out this
 *     field it means the value will be preserved across save/load.
 *     But older versions of R will interpret the entire gp field as
 *     the MISSING field, which may cause confusion.  If we keep this
 *     code, then we will need to make sure that there are no
 *     locked/active bindings in workspaces written for older versions
 *     of R to read.
 *
 * LT */

/** @file envir.cpp
 *
 * Environments: all the action of associating values with symbols
 * happens in this code.
 */

#define R_NO_REMAP

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <functional>
#include <iostream>
#include <Defn.h>
#include <Localization.h>
#include <Internal.h>
#include <R_ext/Callbacks.h>
#include <rho/ClosureContext.hpp>
#include <rho/Frame.hpp>
#include <rho/ListVector.hpp>
#include <rho/Promise.hpp>
#include <rho/ProvenanceTracker.hpp>
#include <rho/StringVector.hpp>
#include <rho/BuiltInFunction.hpp>

using namespace rho;

namespace {
    inline bool FRAME_IS_LOCKED(SEXP e)
    {
	return SEXP_downcast<Environment*>(e)->frame()->isLocked();
    }
}

static SEXP getActiveValue(SEXP fun)
{
    Expression* expr = new Expression(fun, {});
    return expr->evaluate(Environment::global());
}

static void setActiveValue(SEXP fun, SEXP val)
{
    static Symbol* s_quote = Symbol::obtain("quote");
    Expression* quoted_val = new Expression(s_quote, { val });
    Expression* expr = new Expression(fun, { quoted_val });
    expr->evaluate(Environment::global());
}

void Frame::Binding::assignSlow(RObject *new_value, Origin origin)
{
    if (isLocked())
        Rf_error(_("cannot change value of locked binding for '%s'"), symbol()->name()->c_str());
    m_origin = origin;
    if (isActive())
    {
        setActiveValue(m_value, new_value);
        m_frame->monitorRead(*this);
    }
    else
    {
        m_value = new_value;
        m_frame->monitorWrite(*this);
    }
}

RObject* Frame::Binding::unforcedValue() const
{
    RObject* ans = (isActive() ? getActiveValue(m_value) : m_value);
    m_frame->monitorRead(*this);
    return ans;
}

/*----------------------------------------------------------------------

  String Hashing

  This is taken from the second edition of the "Dragon Book" by
  Aho, Ullman and Sethi.

*/

/* was extern: used in this file and names.cpp (for the symbol table).

   This hash function seems to work well enough for symbol tables,
   and hash tables get saved as part of environments so changing it
   is a major decision.
 */
HIDDEN int R_Newhashpjw(const char *s)
{
    const char *p;
    unsigned h = 0, g;
    for (p = s; *p; p++) {
	h = (h << 4) + (*p);
	if ((g = h & 0xf0000000) != 0) {
	    h = h ^ (g >> 24);
	    h = h ^ g;
	}
    }
    return h;
}

/*----------------------------------------------------------------------

  R_NewHashedEnv

*/

SEXP R_NewHashedEnv(SEXP enclos, SEXP size)
{
    GCStackRoot<Environment> enc(SEXP_downcast<Environment*>(enclos));
    GCStackRoot<Frame> frame(Frame::normalFrame());
    return new Environment(enc, frame);
}

/*----------------------------------------------------------------------

  Environments

  The following code implements variable searching for environments.

*/


/*----------------------------------------------------------------------

  InitGlobalEnv

  Create the initial global environment.  The global environment is
  no longer a linked list of environment frames.  Instead it is a
  vector of environments which is searched from beginning to end.

  Note that only the first frame of each of these environments is
  searched.  This is intended to make it possible to implement
  namespaces at some (indeterminate) point in the future.

  We hash the initial environment.
*/

static GCRoot<> R_BaseNamespaceName;
static GCRoot<> R_NamespaceSymbol;

void Rf_InitGlobalEnv()
{
    Environment::initialize();
    R_NamespaceSymbol = Rf_install(".__NAMESPACE__.");
    R_MethodsNamespace = R_GlobalEnv; // so it is initialized.
    SET_SYMVALUE(Rf_install(".BaseNamespaceEnv"), R_BaseNamespace);
    R_BaseNamespaceName = Rf_ScalarString(Rf_mkChar("base"));
    R_PreserveObject(R_BaseNamespaceName);
    GCStackRoot<> zero(Rf_ScalarInteger(0));
    R_NamespaceRegistry = R_NewHashedEnv(nullptr, zero);
    R_PreserveObject(R_NamespaceRegistry);
    Rf_defineVar(Symbols::BaseSymbol, R_BaseNamespace, R_NamespaceRegistry);

#ifdef PROVENANCE_TRACKING
    ProvenanceTracker::setMonitors();
    //ProvenanceTracker::initEnv(SEXP_downcast<Environment*>(R_BaseEnv));
    /**** needed to properly initialize the base namespace */
#endif
}


/*----------------------------------------------------------------------

  unbindVar

  Remove a value from an environment. This happens only in the frame
  of the specified environment.

  FIXME ? should this also unbind the symbol value slot when rho is
  R_BaseEnv.
  This is only called from eval.cpp in applydefine
  (and applydefine only works for unhashed environments, so not base).
*/

HIDDEN void Rf_unbindVar(SEXP symbol, SEXP rho)
{
    if (rho == R_BaseNamespace)
	Rf_error(_("cannot unbind in the base namespace"));
    if (rho == R_BaseEnv)
	Rf_error(_("unbind in the base environment is unimplemented"));

    const Symbol* sym = SEXP_downcast<Symbol*>(symbol);
    Environment* env = SEXP_downcast<Environment*>(rho);
    env->frame()->erase(sym);
}



/*----------------------------------------------------------------------

  findVarLocInFrame

  Look up the location of the value of a symbol in a
  single environment frame.  Almost like findVarInFrame, but
  does not return the value. nullptr if not found.

  Callers set *canCache = TRUE or NULL
*/

// canCache is used for 'user databases': currently (2009-01-18)
// unimplemented in rho.
static R_varloc_t findVarLocInFrame(SEXP rho, SEXP symbol, Rboolean * /*canCache*/)
{
    if (!rho || rho == R_EmptyEnv)
	return(nullptr);

    Environment* env = SEXP_downcast<Environment*>(rho);
    const Symbol* sym = SEXP_downcast<Symbol*>(symbol);
    return env->frame()->binding(sym);
}

/*
  External version and accessor functions. Returned value is cast as
  an opaque pointer to ensure it is only used by routines in this
  group.  This allows the implementation to be changed without needing
  to change other files.
*/

/* used in methods */
R_varloc_t R_findVarLocInFrame(SEXP rho, SEXP symbol)
{
    return findVarLocInFrame(rho, symbol, nullptr);
}

/* used in methods */
Rboolean R_GetVarLocMISSING(R_varloc_t vl)
{
    return Rboolean(vl->origin());
}

/*----------------------------------------------------------------------

  findVarInFrame

  Look up the value of a symbol in a single environment frame.	This
  is the basic building block of all variable lookups.

  It is important that this be as efficient as possible.

  The final argument is usually TRUE and indicates whether the
  lookup is being done in order to get the value (TRUE) or
  simply to check whether there is a value bound to the specified
  symbol in this frame (FALSE).  This is used for get() and exists().
*/

// doGet is used in connection with user databases, currently
// (2009-01-18) unimplemented in rho.
SEXP Rf_findVarInFrame3(SEXP rho, SEXP symbol, Rboolean /*doGet*/)
{
    Environment* env = downcast_to_env(rho);
    Symbol* sym = SEXP_downcast<Symbol*>(symbol);
    Frame::Binding* bdg = env->frame()->binding(sym);
    if (bdg)
	return bdg->unforcedValue();
    // Reproduce the CR behaviour:
    if (sym == Symbol::missingArgument())
	return sym;
    return R_UnboundValue;
}

SEXP Rf_findVarInFrame(SEXP rho, SEXP symbol)
{
    return Rf_findVarInFrame3(rho, symbol, TRUE);
}


/*----------------------------------------------------------------------

  findVar

  Look up a symbol in an environment.

*/

SEXP Rf_findVar(SEXP symbol, SEXP rho)
{
    auto env = downcast_to_env(rho);
    if (!env)
	Rf_error(_("argument to '%s' is not an environment"), "findVar");

    Symbol* sym = SEXP_downcast<Symbol*>(symbol);
    Frame::Binding* bdg = env->findBinding(sym);
    return (bdg ? bdg->unforcedValue() : R_UnboundValue);
}



/*----------------------------------------------------------------------

  findVar1

  Look up a symbol in an environment.  Ignore any values which are
  not of the specified type.

*/

namespace
{
    // Predicate used to test whether a Binding's value is of a
    // specified type.
    class TypeTester : public std::unary_function<RObject *, bool>
    {
    public:
        TypeTester(SEXPTYPE type)
            : m_type(type)
        {
        }

        bool operator()(const RObject *obj);

    private:
        SEXPTYPE m_type;
    };

    bool TypeTester::operator()(const RObject *obj)
    {
        if (m_type == ANYSXP)
            return true;
        if (!obj)
            return m_type == NILSXP;
        if (obj->sexptype() == m_type)
            return true;
        if (m_type == FUNSXP && FunctionBase::isA(obj))
            return true;
        return false;
    }
}

HIDDEN SEXP Rf_findVar1(SEXP symbol, SEXP rho, SEXPTYPE mode, int inherits)
{
    const Symbol* sym = SEXP_downcast<Symbol*>(symbol);
    Environment* env = SEXP_downcast<Environment*>(rho);
    TypeTester typetest(mode);
    RObject* value = findTestedValue(sym, env, typetest, inherits);
    return (value ? value : R_UnboundValue);
}

/*
 *  ditto, but check *mode* not *type*
 */

namespace {
    // Predicate used to test whether a Binding's value is of a
    // specified mode.
    class ModeTester : public std::unary_function<RObject*, bool> {
    public:
	ModeTester(SEXPTYPE mode);

	bool operator()(const RObject* obj);
    private:
	SEXPTYPE m_mode;
    };

    ModeTester::ModeTester(SEXPTYPE mode)
	: m_mode(mode)
    {
	if (mode == INTSXP) m_mode = REALSXP;
	if (mode == CLOSXP || mode == BUILTINSXP || mode == SPECIALSXP)
	    m_mode = FUNSXP;
    }

    bool ModeTester::operator()(const RObject* obj)
    {
	if (m_mode == ANYSXP)
	    return true;
	if (!obj)
	    return m_mode == NILSXP;
	SEXPTYPE ost = obj->sexptype();
	if (ost == INTSXP)
	    ost = REALSXP;
	if (ost == m_mode)
	    return true;
	if (m_mode == FUNSXP && FunctionBase::isA(obj))
	    return true;
	return false;
    }
}

static SEXP findVar1mode(SEXP symbol, SEXP rho, SEXPTYPE mode, int inherits,
             bool doGet)
{
    const Symbol* sym = SEXP_downcast<Symbol*>(symbol);
    Environment* env = SEXP_downcast<Environment*>(rho);
    // For ANYSXP, we use shortcuts to avoid running active binding
    // functions.  cf. comment to existsVarInFrame() in CR.
    if (mode == ANYSXP) {
	Frame::Binding* bdg;
	if (!inherits)
	    bdg = env->frame()->binding(sym);
	else
	    bdg = env->findBinding(sym);

	if (doGet) {
	    return bdg ? bdg->unforcedValue() : R_UnboundValue;
	} else {
	    return bdg ? nullptr : R_UnboundValue;
	}
    }
    ModeTester modetest(mode);
    RObject* value = findTestedValue(sym, env, modetest, inherits);
    return (value ? value : R_UnboundValue);
}


/*
   ddVal ("dot-dot-value"):
   a function to take a name and determine if it is of the form
   ..x where x is an integer; if so x is returned otherwise 0 is returned
*/
static int ddVal(SEXP symbol)
{
    Symbol* sym = SEXP_downcast<Symbol*>(symbol);
    return sym->dotDotIndex();
}

/*----------------------------------------------------------------------
  ddfindVar

  This function fetches the variables ..1, ..2, etc from the first
  frame of the environment passed as the second argument to ddfindVar.
  These variables are implicitly defined whenever a ... object is
  created.

  To determine values for the variables we first search for an
  explicit definition of the symbol, them we look for a ... object in
  the frame and then walk through it to find the appropriate values.

  If no value is obtained we return R_UnboundValue.

  It is an error to specify a .. index longer than the length of the
  ... object the value is sought in.

*/

#define length_DOTS(_v_) (TYPEOF(_v_) == DOTSXP ? Rf_length(_v_) : 0)

SEXP ddfind(int i, SEXP rho)
{
    if(i <= 0)
	Rf_error(_("indexing '...' with non-positive index %d"), i);
    /* first look for ... symbol  */
    SEXP vl = Rf_findVar(Symbols::DotsSymbol, rho);
    if (vl != R_UnboundValue) {
	if (length_DOTS(vl) >= i) {
	    vl = Rf_nthcdr(vl, i - 1);
	    return(CAR(vl));
	}
	else // length(...) < i
	    Rf_error(n_("the ... list does not contain any elements",
			   "the ... list does not contain %d elements", i), i);
    }
    else Rf_error(_("..%d used in an incorrect context, no ... to look in"), i);

    return nullptr;
}

HIDDEN SEXP Rf_ddfindVar(SEXP symbol, SEXP rho)
{
    int i = ddVal(symbol);
    return ddfind(i, rho);
}

HIDDEN SEXP do_dotsElt(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    Rf_check1arg(args, call, "n");

    int i = Rf_asInteger(CAR(args));
    return Rf_eval(ddfind(i, env), env);
}

HIDDEN SEXP do_dotsLength(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP vl = Rf_findVar(Symbols::DotsSymbol, env);
    if (vl == R_UnboundValue)
	Rf_error(_("incorrect context: the current call has no '...' to look in"));
    // else
    return Rf_ScalarInteger(length_DOTS(vl));
}

#undef length_DOTS

/*----------------------------------------------------------------------

  dynamicfindVar

  This function does a variable lookup, but uses dynamic scoping rules
  rather than the lexical scoping rules used in findVar.

  Return R_UnboundValue if the symbol isn't located and the calling
  function needs to handle the errors.

*/

#ifdef UNUSED
SEXP dynamicfindVar(SEXP symbol, ClosureContext *cptr)
{
    SEXP vl;
    while (cptr) {
	vl = Rf_findVarInFrame3(cptr->workingEnvironment(), symbol, TRUE);
	if (vl != R_UnboundValue) return vl;
	cptr = ClosureContext::innermost(cptr->nextOut());
    }
    return R_UnboundValue;
}
#endif



/*----------------------------------------------------------------------

  findFun

  Search for a function in an environment This is a specially modified
  version of findVar which ignores values its finds if they are not
  functions.

 [ NEEDED: This needs to be modified so that a search for an arbitrary mode can
  be made.  Then findVar and findFun could become same function.]

  This could call findVar1.  NB: they behave differently on failure.
*/

HIDDEN SEXP Rf_findFun3(SEXP symbol, SEXP rho, SEXP call)
{
    const Symbol* sym = SEXP_downcast<Symbol*>(symbol);
    Environment* env = SEXP_downcast<Environment*>(rho);
    FunctionBase* fun = findFunction(sym, env);
    if (fun)
	return fun;
    errorcall_cpy(call,
                  _("could not find function \"%s\""),
                  sym->name()->c_str());
    /* NOT REACHED */
    return R_UnboundValue;
}

SEXP Rf_findFun(SEXP symbol, SEXP rho)
{
    FunctionContext *c = FunctionContext::innermost();
    return Rf_findFun3(symbol, rho, c ? const_cast<Expression*>(c->call()) : static_cast<RObject*>(nullptr));
}

/*----------------------------------------------------------------------

  defineVar

  Assign a value in a specific environment frame.

*/

void Rf_defineVar(SEXP symbol, SEXP value, SEXP rho)
{
    if (rho == R_EmptyEnv)
	Rf_error(_("cannot assign values in the empty environment"));

    GCStackRoot<> valrt(value);
    Environment* env = SEXP_downcast<Environment*>(rho);
    const Symbol* sym = SEXP_downcast<Symbol*>(symbol);
    Frame::Binding* bdg = env->frame()->obtainBinding(sym);
    bdg->assign(value);
}


/*----------------------------------------------------------------------

    setVar

    Assign a new value to bound symbol.	 Note this does the "inherits"
    case.  I.e. it searches frame-by-frame for a symbol and binds the
    given value to the first symbol encountered.  If no symbol is
    found then a binding is created in the global environment.

    Changed in R 2.4.0 to look in the base environment (previously the
    search stopped befor the base environment, but would (and still
    does) assign into the base namespace if that is on the search and
    the symbol existed there).

*/

void Rf_setVar(SEXP symbol, SEXP value, SEXP rho)
{
    Symbol* sym = SEXP_downcast<Symbol*>(symbol);
    Environment* env = SEXP_downcast<Environment*>(rho);
    Frame::Binding* bdg = env->findBinding(sym);
    if (!bdg) {
	bdg = Environment::global()->frame()->obtainBinding(sym);
    }
    bdg->assign(value);
}



/** --------------------------------------------------------------------
*
*  gsetVar
*
*  @brief Assignment in the base environment.
*
*  @details Here we assign directly into
*  the base environment.
*
*/

void Rf_gsetVar(SEXP symbol, SEXP value, SEXP rho)
{
    GCStackRoot<> valrt(value);
    const Symbol* sym = SEXP_downcast<Symbol*>(symbol);
    Frame::Binding* bdg = Environment::base()->frame()->obtainBinding(sym);
    bdg->assign(value);
}

namespace rho {
    /* get environment from a subclass if possible; else return NULL */
    Environment* simple_as_environment(RObject* arg, bool allow_null = false) {
	if (!arg) {
	    if (allow_null)
		return nullptr;
	    else
		Environment::nullEnvironmentError();
	}
	if (arg->sexptype() == ENVSXP) {
	    return SEXP_downcast<Environment*>(arg);
	}
	if (IS_S4_OBJECT(arg) && (TYPEOF(arg) == S4SXP)) {
	    return SEXP_downcast<Environment*>(R_getS4DataSlot(arg, ENVSXP));
	}
	return nullptr;
    }
}
/*----------------------------------------------------------------------

  do_assign : .Internal(assign(x, value, envir, inherits))

*/
HIDDEN SEXP do_assign(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* value_, RObject* envir_, RObject* inherits_)
{
    SEXP name=nullptr, val, aenv;
    int ginherits = 0;

    if (!Rf_isString(x_) || Rf_length(x_) == 0)
	Rf_error(_("invalid first argument"));
    else {
	if (Rf_length(x_) > 1)
	    Rf_warning(_("only the first element is used as variable name"));
	name = Rf_installTrChar(STRING_ELT(x_, 0));
    }
    PROTECT(val = value_);
    aenv = simple_as_environment(envir_);
    if (!aenv)
	Rf_error(_("invalid '%s' argument"), "envir");
    ginherits = Rf_asLogical(inherits_);
    if (ginherits == R_NaLog)
	Rf_error(_("invalid '%s' argument"), "inherits");
    if (ginherits)
	Rf_setVar(name, val, aenv);
    else
	Rf_defineVar(name, val, aenv);
    UNPROTECT(1);
    return val;
}


/**
 * do_list2env : .Internal(list2env(x, envir))
  */
HIDDEN SEXP do_list2env(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* envir_)
{
    SEXP x, xnms, envir;
    int n;

    if (TYPEOF(x_) != VECSXP)
	Rf_error(_("first argument must be a named list"));
    x = x_;
    n = LENGTH(x);
    xnms = Rf_getAttrib(x, Symbols::NamesSymbol);
    if (n && (TYPEOF(xnms) != STRSXP || LENGTH(xnms) != n))
	Rf_error(_("names(x) must be a character vector of the same length as x"));
    envir = envir_;
    if (TYPEOF(envir) != ENVSXP)
	Rf_error(_("'envir' argument must be an environment"));

    for(int i = 0; i < n; i++) {
	SEXP name = Rf_installTrChar(STRING_ELT(xnms, i));
	Rf_defineVar(name, VECTOR_ELT(x, i), envir);
    }

    return envir;
}


/** ----------------------------------------------------------------------

  do_remove

  @brief Remove variable

  @details There are three arguments to do_remove; a list of names to remove,
  an optional environment (if missing set it to R_GlobalEnv) and
  inherits, a logical indicating whether to look in the parent env if
  a symbol is not found in the supplied env.  This is ignored if
  environment is not specified.

*/

static bool RemoveVariable(rho::Symbol* name, rho::Environment* env)
{
    if (env == R_BaseNamespace)
	Rf_error(_("cannot remove variables from base namespace"));
    if (env == R_BaseEnv)
	Rf_error(_("cannot remove variables from the base environment"));
    if (env == R_EmptyEnv)
	Rf_error(_("cannot remove variables from the empty environment"));
    if (FRAME_IS_LOCKED(env))
	Rf_error(_("cannot remove bindings from a locked environment"));

    Environment* envir = SEXP_downcast<Environment*>(env);
    const Symbol* sym = SEXP_downcast<Symbol*>(name);
    bool found = envir->frame()->erase(sym);
    return found;
}

HIDDEN SEXP do_remove(/*const*/ Expression* call, const BuiltInFunction* op, RObject* list_, RObject* envir_, RObject* inherits_)
{
    /* .Internal(remove(list, envir, inherits)) */

    rho::RObject* name;
    rho::Environment* envarg;
    rho::Symbol* tsym;
    rho::Environment* tenv;
    int ginherits = 0;
    bool done;

    name = list_;
    if (!Rf_isString(name))
	Rf_error(_("invalid first argument"));

    envarg = simple_as_environment(envir_);
    if (!envarg)
	Rf_error(_("invalid '%s' argument"), "envir");

    ginherits = Rf_asLogical(inherits_);
    if (ginherits == R_NaLog)
	Rf_error(_("invalid '%s' argument"), "inherits");

    for (int i = 0; i < LENGTH(name); i++) {
	done = 0;
	tsym = SEXP_downcast<Symbol*>(rho::Symbol::obtain(Rf_translateChar(STRING_ELT(name, i))));
	tenv = envarg;
	while (tenv != R_EmptyEnv) {
	    done = RemoveVariable(tsym, tenv);
	    if (done || !ginherits)
		break;
	    tenv =  SEXP_downcast<Environment*>(CDR(static_cast<SEXP>(tenv)));
	}
	if (!done)
	    Rf_warning(_("object '%s' not found"), Rf_EncodeChar(PRINTNAME(tsym)));
    }
    return nullptr;
}


/** ----------------------------------------------------------------------

  Used by do_get, do_get0

  @brief This function returns the SEXP associated with the character
  argument.

  @param call  environment of the calling function as a
  default.

  @return Returns R_UnboundValue if no value was found.
*/

static RObject* do_get_common(Expression* call, const BuiltInFunction* op,
			      RObject* x, RObject* envir, RObject* mode,
			      RObject* inherits)
{
    SEXP rval, genv, t1 = nullptr;
    SEXPTYPE gmode;
    int ginherits = 0, where;

    /* The first arg is the object name */
    /* It must be present and a non-empty string */

    if (!Rf_isValidStringF(x))
	Rf_error(_("invalid first argument"));
    else
	t1 = Rf_installTrChar(STRING_ELT(x, 0));

    /* envir :	originally, the "where=" argument */

    if (TYPEOF(envir) == REALSXP || TYPEOF(envir) == INTSXP) {
	where = Rf_asInteger(envir);
	genv = R_sysframe(where, R_GlobalContext());
    }
    else {
	genv = simple_as_environment(envir);
	if (!genv)
	    Rf_error(_("invalid '%s' argument"), "envir");
    }

    /* mode :  The mode of the object being sought */

    /* as from R 1.2.0, this is the *mode*, not the *typeof* aka
       storage.mode.
    */

    if (Rf_isString(mode)) {
	if (streql(R_CHAR(STRING_ELT(mode, 0)), "function")) /* ASCII */
	    gmode = FUNSXP;
	else
	    gmode = Rf_str2type(R_CHAR(STRING_ELT(mode, 0))); /* ASCII */
    } else {
	Rf_error(_("invalid '%s' argument"), "mode");
	gmode = FUNSXP;/* -Wall */
    }

    ginherits = Rf_asLogical(inherits);
    if (ginherits == R_NaLog)
	Rf_error(_("invalid '%s' argument"), "inherits");

    /* Search for the object */
    rval = findVar1mode(t1, genv, gmode, ginherits, op->variant());
    if (rval == R_MissingArg)
	Rf_error(_("argument \"%s\" is missing, with no default"),
	      R_CHAR(PRINTNAME(t1)));

    return rval;
}

HIDDEN SEXP do_get(Expression* call, const BuiltInFunction* op,
			     RObject* x, RObject* envir, RObject* mode,
			     RObject* inherits)
{
    RObject* value = do_get_common(call, op,
				   x, envir, mode, inherits);
    switch (op->variant()) {
    case 0: // exists(.) :
	return Rf_ScalarLogical(value != R_UnboundValue);
    case 1: //  have get(.)
	if (value == R_UnboundValue) {
	    SEXP x_printname = PRINTNAME(Rf_installTrChar(STRING_ELT(x, 0)));
	    if (Rf_str2type(R_CHAR(STRING_ELT(mode, 0))) == ANYSXP)
		Rf_error(_("object '%s' not found"), Rf_EncodeChar(x_printname));
	    else
		Rf_error(_("object '%s' of mode '%s' was not found"),
		      R_CHAR(x_printname),
		      R_CHAR(STRING_ELT(mode, 0))); /* ASCII */
	}
	return forceIfPromise(value);
    default:
	Rf_error(_("unknown op"));
    }
}

HIDDEN SEXP do_get0(Expression* call, const BuiltInFunction* op,
			      RObject* x, RObject* envir, RObject* mode,
			      RObject* inherits, RObject* value_if_missing)
{
    RObject* value = do_get_common(call, op,
				   x, envir, mode, inherits);
    if (value != R_UnboundValue)
	return forceIfPromise(value);
    return value_if_missing;
}

static SEXP gfind(const char *name, Environment* env, SEXPTYPE mode,
		  SEXP ifnotfound, int inherits)
{
    Symbol* t1 = Symbol::obtain(name);

    /* Search for the object - last arg is 1 to 'get' */
    SEXP rval = findVar1mode(t1, env, mode, inherits, true);

    if (rval == R_UnboundValue) {
	if( Rf_isFunction(ifnotfound) ) {
	    SEXP var = Rf_mkString(name);
	    Expression* R_fcall = new Expression(ifnotfound, { var });
	    rval = R_fcall->evaluate(Environment::base());
	    UNPROTECT(1);
	} else
	    rval = ifnotfound;
    }

    /* We need to evaluate if it is a promise */
    if (TYPEOF(rval) == PROMSXP) rval = rval->evaluate(env);
    ENSURE_NAMED(rval);
    return rval;
}


/** mget():
 * 
 * @brief get multiple values from an environment
 *
 * .Internal(mget(x, envir, mode, ifnotfound, inherits))
 *
 * @return  a list of the same length as x, a character vector (of names).
 */
HIDDEN SEXP do_mget(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x, RObject* envir, RObject* mode, RObject* ifnotfound, RObject* inherits)
{
    SEXP ans;
    int ginherits = 0, nvals, nmode, nifnfnd;

    nvals = Rf_length(x);

    /* The first arg is the object name */
    /* It must be present and a string */
    if (!Rf_isString(x) )
	Rf_error(_("invalid first argument"));
    for(int i = 0; i < nvals; i++)
	if( Rf_isNull(STRING_ELT(x, i)) || !R_CHAR(STRING_ELT(x, 0))[0] )
	    Rf_error(_("invalid name in position %d"), i+1);

    Environment* env = downcast_to_env(envir);
    if (!env)
	Rf_error(_("second argument must be an environment"));

    nmode = Rf_length(mode);
    if( !Rf_isString(mode) )
	Rf_error(_("invalid '%s' argument"), "mode");

    if( nmode != nvals && nmode != 1 )
	Rf_error(_("wrong length for '%s' argument"), "mode");

    PROTECT(ifnotfound = Rf_coerceVector(ifnotfound, VECSXP));
    nifnfnd = Rf_length(ifnotfound);
    if( !Rf_isVector(ifnotfound) )
	Rf_error(_("invalid '%s' argument"), "ifnotfound");

    if( nifnfnd != nvals && nifnfnd != 1 )
	Rf_error(_("wrong length for '%s' argument"), "ifnotfound");

    ginherits = Rf_asLogical(inherits);
    if (ginherits == R_NaLog)
	Rf_error(_("invalid '%s' argument"), "inherits");

    PROTECT(ans = Rf_allocVector(VECSXP, nvals));

    for(int i = 0; i < nvals; i++) {
	SEXPTYPE gmode;
	if (streql(R_CHAR(STRING_ELT(mode, i % nmode)), "function"))
	    gmode = FUNSXP;
	else {
	    gmode = Rf_str2type(R_CHAR(STRING_ELT(mode, i % nmode)));
	    if(gmode == SEXPTYPE( (-1)))
		Rf_error(_("invalid '%s' argument"), "mode");
	}
	SEXP ans_i = gfind(Rf_translateChar(STRING_ELT(x, i % nvals)), env,
                           gmode, VECTOR_ELT(ifnotfound, i % nifnfnd),
                           ginherits);
	SET_VECTOR_ELT(ans, i, Rf_lazy_duplicate(ans_i));
    }

    Rf_setAttrib(ans, Symbols::NamesSymbol, Rf_lazy_duplicate(x));
    UNPROTECT(2);
    return(ans);
}

/** ----------------------------------------------------------------------

  do_missing

  @brief This function tests whether the symbol passed as its first argument
  is a missing argument to the current closure.  rho is the
  environment that missing was called from.

  @details R_isMissing is called on the not-yet-evaluated value of an argument,
  if this is a symbol, as it could be a missing argument that has been
  passed down.  So 'symbol' is the promise value, and 'rho' its
  evaluation argument.

  It is also called in arithmetic.cpp. for e.g. do_log
*/

static SEXP findRootPromise(SEXP p)
{
    if (TYPEOF(p) == PROMSXP) {
	while (TYPEOF(PREXPR(p)) == PROMSXP) {
	    p = PREXPR(p);
	}
    }
    return p;
}

HIDDEN int R_isMissing(SEXP symbol, SEXP rho)
{
    if (!rho)
	return 0;
    Symbol* sym = SEXP_downcast<Symbol*>(symbol);
    Environment* env = SEXP_downcast<Environment*>(rho);
    return isMissingArgument(sym, env->frame());
}

/* this is primitive and a SPECIALSXP */
HIDDEN SEXP do_missing(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int ddv=0;
    SEXP sym, s;

    GCStackRoot<> t;  // Binding defined in PairList form

    s = sym = CAR(args);
    if( Rf_isString(sym) && Rf_length(sym)==1 )
	s = sym = Rf_installTrChar(STRING_ELT(CAR(args), 0));
    if (!Rf_isSymbol(sym))
	Rf_errorcall(call, _("invalid use of 'missing'"));

    if (DDVAL(sym)) {
	ddv = ddVal(sym);
	sym = Symbols::DotsSymbol;
    }

    Frame::Binding* bdg = findVarLocInFrame(rho, sym, nullptr);
    t = (bdg ? bdg->asPairList() : nullptr);
    if (t != nullptr) {
	if (DDVAL(s)) {
	    if (Rf_length(CAR(t)) < ddv  || CAR(t) == R_MissingArg) {
		return Rf_ScalarLogical(1);
	    }
	    else
		t = Rf_nthcdr(CAR(t), ddv-1);
	}
	if (MISSING(t) || CAR(t) == R_MissingArg) {
	    return Rf_ScalarLogical(1);
	}
	else goto havebinding;
    }
    else  /* it wasn't an argument to the function */
	Rf_errorcall(call, _("'missing' can only be used for arguments"));

 havebinding:

    t = CAR(t);
    if (TYPEOF(t) != PROMSXP) {
	return Rf_ScalarLogical(0);
    }

    t = findRootPromise(t);
    if (!Rf_isSymbol(PREXPR(t)))
	return Rf_ScalarLogical(0);
    else
	return Rf_ScalarLogical(R_isMissing(PREXPR(t), PRENV(t)));
}

/** ----------------------------------------------------------------------

  @name do_globalenv

  @return Returns the current global environment.

*/

HIDDEN SEXP do_globalenv(/*const*/ Expression* call, const BuiltInFunction* op)
{
    return R_GlobalEnv;
}

/** ----------------------------------------------------------------------

  @name do_baseenv

  @return Returns the current base environment.

*/

HIDDEN SEXP do_baseenv(/*const*/ Expression* call, const BuiltInFunction* op)
{
    return R_BaseEnv;
}

/** ----------------------------------------------------------------------

  @name do_emptyenv

  @return Returns the current empty environment.

*/


HIDDEN SEXP do_emptyenv(/*const*/ Expression* call, const BuiltInFunction* op)
{
    return R_EmptyEnv;
}


/** ----------------------------------------------------------------------

  @name do_attach

  @details To attach a list we make up an environment and insert components
  of the list in as the values of this env and install the tags from
  the list as the names.

*/

HIDDEN SEXP do_attach(/*const*/ Expression* call, const BuiltInFunction* op, RObject* what_, RObject* pos_, RObject* name_)
{
    GCStackRoot<Environment> env_to_attach;

    int pos = Rf_asInteger(pos_);
    if (pos == R_NaInt)
	Rf_error(_("'pos' must be an integer"));

    if (!Rf_isValidStringF(name_))
	Rf_error(_("invalid '%s' argument"), "name");
    StringVector* name = SEXP_downcast<StringVector*>(name_);

    if (Rf_isNewList(what_)) {
        GCStackRoot<Frame> frame(Frame::normalFrame());
	GCStackRoot<Environment> newenv(new Environment(nullptr, frame));

	const ListVector* elements = SEXP_downcast<ListVector*>(what_);
	if (elements)
	{
	    const StringVector* names = elements->names();
	    size_t length = elements->size();
	    if (!names || elements->size() != names->size()) {
		Rf_error(_("all elements of a list must be named"));
	    }
	    for (size_t i = 0; i < length; ++i) {
		if (!(*names)[i]) {
		    Rf_error(_("all elements of a list must be named"));
		}
		frame->bind(Symbol::obtain((*names)[i]), (*elements)[i]);
	    }
	}
	env_to_attach = newenv;
    } else if (Rf_isEnvironment(what_)) {
	env_to_attach = SEXP_downcast<Environment*>(what_);
    } else {
	Rf_error(_("'attach' only works for lists, data frames and environments"));
    }
    return env_to_attach->attachToSearchPath(pos, name);
}


/** ----------------------------------------------------------------------

  @name do_detach

  @brief detach the specified environment.  Detachment only takes place by
  position.

*/

HIDDEN SEXP do_detach(/*const*/ Expression* call, const BuiltInFunction* op, RObject* pos_)
{
    int pos = Rf_asInteger(pos_);

    return Environment::detachFromSearchPath(pos);
}



/** ----------------------------------------------------------------------

  @name do_search

  @brief Print out the current search path.

*/

HIDDEN SEXP do_search(/*const*/ Expression* call, const BuiltInFunction* op)
{
    SEXP ans, name, t;
    int i, n;

    n = 2;
    for (t = ENCLOS(R_GlobalEnv); t != R_BaseEnv ; t = ENCLOS(t))
	n++;
    PROTECT(ans = Rf_allocVector(STRSXP, n));
    /* TODO - what should the name of this be? */
    SET_STRING_ELT(ans, 0, Rf_mkChar(".GlobalEnv"));
    SET_STRING_ELT(ans, n-1, Rf_mkChar("package:base"));
    i = 1;
    for (t = ENCLOS(R_GlobalEnv); t != R_BaseEnv ; t = ENCLOS(t)) {
	name = Rf_getAttrib(t, Symbols::NameSymbol);
	if (!Rf_isString(name) || Rf_length(name) < 1)
	    SET_STRING_ELT(ans, i, Rf_mkChar("(unknown)"));
	else
	    SET_STRING_ELT(ans, i, STRING_ELT(name, 0));
	i++;
    }
    UNPROTECT(1);
    return ans;
}


/*----------------------------------------------------------------------

  do_ls

  This code implements the functionality of the "ls" and "objects"
  functions.  [ ls(envir, all.names, sorted) ]

*/

static int FrameSize(SEXP frame, int all)
{
    int count = 0;
    while (frame != nullptr) {
	if ((all || R_CHAR(PRINTNAME(TAG(frame)))[0] != '.') &&
				      CAR(frame) != R_UnboundValue)
	    count += 1;
	frame = CDR(frame);
    }
    return count;
}

static void FrameNames(SEXP frame, int all, SEXP names, int *indx)
{
    while (frame != nullptr) {
	if ((all || R_CHAR(PRINTNAME(TAG(frame)))[0] != '.') &&
				      CAR(frame) != R_UnboundValue) {
	    SET_STRING_ELT(names, *indx, PRINTNAME(TAG(frame)));
	    (*indx)++;
	}
	frame = CDR(frame);
    }
}

static void FrameValues(SEXP frame, int all, SEXP values, int *indx)
{
    while (frame != nullptr) {
	if ((all || R_CHAR(PRINTNAME(TAG(frame)))[0] != '.') &&
				      CAR(frame) != R_UnboundValue) {
	    SEXP value = CAR(frame);
	    if (TYPEOF(value) == PROMSXP) {
		PROTECT(value);
		value = Rf_eval(value, R_GlobalEnv);
		UNPROTECT(1);
	    }
	    SET_VECTOR_ELT(values, *indx, Rf_lazy_duplicate(value));
	    (*indx)++;
	}
	frame = CDR(frame);
    }
}

static bool BuiltinTest(const Symbol* sym, bool all, bool internal_only)
{
    if ((sym->name()->c_str()[0] == '.') && !all) {
	return false;
    }
    if (internal_only) {
	return BuiltInFunction::obtainInternal(sym);
    }
    if (SYMVALUE(const_cast<Symbol*>(sym)) != R_UnboundValue) {
	return true;
    }
    return false;
}

static int BuiltinSize(int all, int intern)
{
    int count = 0;
    Symbol::const_iterator end = Symbol::end();
    for (Symbol::const_iterator it = Symbol::begin(); it != end; ++it) {
	const Symbol* sym = *it;
	if (BuiltinTest(sym, all, intern))
	    ++count;
    }
    return count;
}

static void BuiltinNames(int all, int intern, SEXP names, int *indx)
{
    StringVector* sv = SEXP_downcast<StringVector*>(names);
    Symbol::const_iterator end = Symbol::end();
    for (Symbol::const_iterator it = Symbol::begin(); it != end; ++it) {
	const Symbol* sym = *it;
	if (BuiltinTest(sym, all, intern))
	    (*sv)[(*indx)++] = const_cast<String*>(sym->name());
    }
}

// .Internal(ls(envir, all.names, sorted)) :
HIDDEN SEXP do_ls(/*const*/ Expression* call, const BuiltInFunction* op, RObject* envir_, RObject* all_names_, RObject* sorted_)
{
    RObject* env = envir_;

    int all = Rf_asLogical(all_names_);
    if (all == R_NaLog) all = 0;

    int sort_nms = Rf_asLogical(sorted_); /* sorted = TRUE/FALSE */
    if (sort_nms == R_NaLog) sort_nms = 0;

    return R_lsInternal3(env, Rboolean(all), Rboolean(sort_nms));
}

/* takes an environment, a boolean indicating whether to get all
   names and a boolean if sorted is desired */
SEXP R_lsInternal3(SEXP env, Rboolean all, Rboolean sorted)
{
    const Environment* envir
	= simple_as_environment(env, /* allow_null = */ true);
    if (!envir)
	Rf_error(_("invalid '%s' argument"), "envir");
    std::vector<const Symbol*> syms = envir->frame()->symbols(all);
    std::size_t sz = syms.size();
    GCStackRoot<StringVector> ans(StringVector::create(sz));
    for (unsigned int i = 0; i < sz; ++i)
	(*ans)[i] = const_cast<String*>(syms[i]->name());
    if (sorted)
	Rf_sortVector(ans, FALSE);
    return ans;
}

/* non-API version used in several packages */
SEXP R_lsInternal(SEXP env, Rboolean all)
{
    return R_lsInternal3(env, all, TRUE);
}

/* transform an environment into a named list: as.list.environment(.) */

HIDDEN SEXP do_env2list(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* all_names_, RObject* sorted_)
{
    Environment *env = simple_as_environment(x_);
    if (!env)
        Rf_error(_("argument must be an environment"));

    int all = Rf_asLogical(all_names_); /* all.names = TRUE/FALSE */
    if (all == R_NaLog)
        all = 0;

    int sort_nms = Rf_asLogical(sorted_); /* sorted = TRUE/FALSE */
    if (sort_nms == R_NaLog)
        sort_nms = 0;

    GCStackRoot<Frame> frame(env->frame());
    std::vector<const Symbol *> syms = frame->symbols(all, sort_nms);

    ListVector *result = ListVector::create(syms.size());
    StringVector *names = StringVector::create(syms.size());
    for (size_t i = 0; i < syms.size(); ++i)
    {
        const Symbol *symbol = syms[i];
        (*names)[i] = const_cast<String *>(symbol->name());
        (*result)[i] = frame->binding(symbol)->forcedValue();
    }
    if (syms.size() > 0)
        Rf_setAttrib(result, Symbols::NamesSymbol, names);
    return (result);
}

/*
 * apply a function to all objects in an environment and return the
 * results in a list.
 * Equivalent to lapply(as.list(env, all.names=all.names), FUN, ...)
 */
/* This is a special .Internal */
HIDDEN SEXP do_eapply(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP env, ans, FUN, tmp, tmp2, ind;
    int i, k, k2;
    int /* boolean */ all, useNms;

    env = downcast_to_env(Rf_eval(CAR(args), rho));
    if(!env)
	Rf_error(_("argument must be an environment"));

    FUN = CADR(args);
    if (!Rf_isSymbol(FUN))
	Rf_error(_("arguments must be symbolic"));

    /* 'all.names' : */
    all = Rf_asLogical(Rf_eval(CADDR(args), rho));
    if (all == R_NaLog) all = 0;

    /* 'USE.NAMES' : */
    useNms = Rf_asLogical(Rf_eval(CADDDR(args), rho));
    if (useNms == R_NaLog) useNms = 0;

    GCStackRoot<> framelist(FRAME(env));

    k = FrameSize(framelist, all);

    PROTECT(ans  = Rf_allocVector(VECSXP, k));
    PROTECT(tmp2 = Rf_allocVector(VECSXP, k));

    k2 = 0;
    FrameValues(framelist, all, tmp2, &k2);

    static Symbol* Xsym = Symbol::obtain("X");
    static Symbol* isym = Symbol::obtain("i");

    PROTECT(ind = Rf_allocVector(INTSXP, 1));
    /* tmp :=  `[`(<elist>, i) */
    PROTECT(tmp = new Expression(Symbols::Bracket2Symbol, { Xsym, ind }));
    /* fcall :=  <FUN>(tmp, ... ) */
    Expression* R_fcall = new Expression(FUN, { tmp, Symbols::DotsSymbol });

    Rf_defineVar(Xsym, tmp2, rho);
    INCREMENT_NAMED(tmp2);
    Rf_defineVar(isym, ind, rho);
    INCREMENT_NAMED(ind);

    for(i = 0; i < k2; i++) {
	INTEGER(ind)[0] = i+1;
	SEXP tmp = R_forceAndCall(R_fcall, 1, rho);
	if (MAYBE_REFERENCED(tmp))
	    tmp = Rf_lazy_duplicate(tmp);
	SET_VECTOR_ELT(ans, i, tmp);
    }

    if (useNms) {
	SEXP names;
	PROTECT(names = Rf_allocVector(STRSXP, k));
	k = 0;
	FrameNames(framelist, all, names, &k);

	Rf_setAttrib(ans, Symbols::NamesSymbol, names);
	UNPROTECT(1);
    }
    UNPROTECT(4);
    return(ans);
}

R_xlen_t Rf_envxlength(SEXP rho)
{
    const Environment* env = SEXP_downcast<Environment*>(rho);
    return env->frame()->size();
}

/*----------------------------------------------------------------------

  do_builtins

  Return the names of all the built in functions.  These are fetched
  directly from the symbol table.

*/

HIDDEN SEXP do_builtins(/*const*/ Expression* call, const BuiltInFunction* op, RObject* internal_)
{
    SEXP ans;
    int intern, nelts;
    intern = Rf_asLogical(internal_);
    if (intern == R_NaInt) intern = 0;
    nelts = BuiltinSize(1, intern);
    ans = Rf_allocVector(STRSXP, nelts);
    nelts = 0;
    BuiltinNames(1, intern, ans, &nelts);
    Rf_sortVector(ans, TRUE);
    return ans;
}


/*----------------------------------------------------------------------

  do_pos2env

  This function returns the environment at a specified position in the
  search path or the environment of the caller of
  pos.to.env (? but pos.to.env is usually used in arg lists and hence
  is evaluated in the calling environment so this is one higher).

  When pos = -1 the environment of the closure that pos2env is
  evaluated in is obtained. Note: this relies on pos.to.env being
  a primitive.

 */
static SEXP pos2env(int pos, SEXP call)
{
    SEXP env;
    ClosureContext *cptr;

    if (pos == R_NaInt || pos < -1 || pos == 0) {
	Rf_errorcall(call, _("invalid '%s' argument"), "pos");
	env = call;/* just for -Wall */
    }
    else if (pos == -1) {
	/* make sure the context is a funcall */
	cptr = R_GlobalContext();
	if( !cptr )
	    Rf_errorcall(call, _("no enclosing environment"));

	env = cptr->callEnvironment();
	if (R_GlobalEnv != nullptr && env == nullptr)
	    Rf_errorcall(call, _("invalid '%s' argument"), "pos");
    }
    else {
	for (env = R_GlobalEnv; env != R_EmptyEnv && pos > 1;
	     env = ENCLOS(env))
	    pos--;
	if (pos != 1)
	    Rf_errorcall(call, _("invalid '%s' argument"), "pos");
    }
    return env;
}

/* this is primitive */
HIDDEN SEXP do_pos2env(/*const*/ Expression* call, const BuiltInFunction* op, RObject* pos)
{
    SEXP env;
    int i, npos;

    PROTECT(pos = Rf_coerceVector(pos, INTSXP));
    npos = Rf_length(pos);
    if (npos <= 0)
	Rf_errorcall(call, _("invalid '%s' argument"), "pos");
    PROTECT(env = Rf_allocVector(VECSXP, npos));
    for (i = 0; i < npos; i++) {
	SET_VECTOR_ELT(env, i, pos2env(INTEGER(pos)[i], call));
    }
    if (npos == 1) env = VECTOR_ELT(env, 0);
    UNPROTECT(2);
    return env;
}

static SEXP matchEnvir(SEXP call, const char *what)
{
    SEXP t, name;
    const void *vmax = vmaxget();
    if(streql(".GlobalEnv", what))
	return R_GlobalEnv;
    if(streql("package:base", what))
	return R_BaseEnv;
    for (t = ENCLOS(R_GlobalEnv); t != R_EmptyEnv ; t = ENCLOS(t)) {
	name = Rf_getAttrib(t, Symbols::NameSymbol);
	if(Rf_isString(name) && Rf_length(name) > 0 &&
	   streql(Rf_translateChar(STRING_ELT(name, 0)), what)) {
	    vmaxset(vmax);
	    return t;
	}
    }
    Rf_errorcall(call, _("no item called \"%s\" on the search list"), what);
    /* not reached */
    vmaxset(vmax);
    return nullptr;
}

/* This is primitive */
HIDDEN SEXP do_as_environment(/*const*/ Expression* call, const BuiltInFunction* op, RObject* arg)
{
    if(Rf_isEnvironment(arg))
	return arg;

    switch(TYPEOF(arg)) {
    case STRSXP:
	return matchEnvir(call, Rf_translateChar(Rf_asChar(arg)));
    case REALSXP:
    case INTSXP:
	return do_pos2env(call, op, arg);
    case NILSXP:
	Rf_errorcall(call,_("using 'as.environment(NULL)' is defunct"));
	return R_BaseEnv;	/* -Wall */
    case S4SXP: {
	/* dispatch was tried above already */
	Environment* dot_xData = simple_as_environment(arg);
	if(!dot_xData)
	    Rf_errorcall(call, _("S4 object does not extend class \"environment\""));
	return(dot_xData);
    }
    case VECSXP: {
	/* implement as.environment.list() {Rf_isObject(.) is false for a list} */
	SEXP call, val;
	PROTECT(call = Rf_lang4(Rf_install("list2env"), arg,
			     /* envir = */nullptr,
			     /* parent = */R_EmptyEnv));
	val = Rf_eval(call, R_BaseEnv);
	UNPROTECT(1);
	return val;
    }
    default:
	Rf_errorcall(call, _("invalid object for 'as.environment'"));
	return nullptr;	/* -Wall */
    }
}

void R_LockEnvironment(SEXP env, Rboolean bindings)
{
    Environment* envir = simple_as_environment(env, /* allow_null = */ true);
    if (!envir)
	Rf_error(_("not an environment"));

    if (envir == R_BaseEnv || envir == R_BaseNamespace) {
	if (bindings) {
	    Environment::base()->frame()->lockBindings();
	}
#ifdef NOT_YET
	/* causes problems with Matrix */
	LOCK_FRAME(envir);
#endif
	return;
    }

    envir->frame()->lock(bindings);
}

Rboolean R_EnvironmentIsLocked(SEXP env)
{
    Environment* envir = simple_as_environment(env);
    if (!envir)
	Rf_error(_("not an environment"));
    return Rboolean(envir->frame()->isLocked());
}

SEXP do_lockEnv(/*const*/ Expression* call, const BuiltInFunction* op, RObject* env_, RObject* bindings_)
{
    SEXP frame;
    Rboolean bindings;
    frame = env_;
    bindings = Rboolean(Rf_asLogical(bindings_));
    R_LockEnvironment(frame, bindings);
    return nullptr;
}

HIDDEN SEXP do_envIsLocked(/*const*/ Expression* call, const BuiltInFunction* op, RObject* env_)
{
    return Rf_ScalarLogical(R_EnvironmentIsLocked(env_));
}

void R_LockBinding(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	Rf_error(_("not a symbol"));
    Environment* envir = simple_as_environment(env);
    if (!envir)
	Rf_error(_("not an environment"));
    const Symbol* symbol = SEXP_downcast<Symbol*>(sym);
    Frame::Binding* binding = envir->frame()->binding(symbol);
    if (!binding)
	Rf_error(_("no binding for \"%s\""), symbol->name()->c_str());
    binding->setLocking(true);
}

void R_unLockBinding(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	Rf_error(_("not a symbol"));
    Environment* envir = simple_as_environment(env);
    if (!envir)
	Rf_error(_("not an environment"));
    const Symbol* symbol = SEXP_downcast<Symbol*>(sym);
    Frame::Binding* binding = envir->frame()->binding(symbol);
    if (!binding)
	Rf_error(_("no binding for \"%s\""), symbol->name()->c_str());
    binding->setLocking(false);
}

void R_MakeActiveBinding(SEXP sym, SEXP fun, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	Rf_error(_("not a symbol"));
    if (! Rf_isFunction(fun))
	Rf_error(_("not a function"));
    Environment* envir = simple_as_environment(env);
    if (!envir)
	Rf_error(_("not an environment"));
    const Symbol* symbol = SEXP_downcast<Symbol*>(sym);
    Frame::Binding* binding = envir->frame()->obtainBinding(symbol);
    FunctionBase* function = static_cast<FunctionBase*>(fun);
    binding->setFunction(function);
}

Rboolean R_BindingIsLocked(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	Rf_error(_("not a symbol"));
    Environment* envir = simple_as_environment(env);
    if (!envir)
	Rf_error(_("not an environment"));
    const Symbol* symbol = SEXP_downcast<Symbol*>(sym);
    Frame::Binding* binding = envir->frame()->binding(symbol);
    if (!binding)
	Rf_error(_("no binding for \"%s\""), symbol->name()->c_str());
    return Rboolean(binding->isLocked());
}

Rboolean R_BindingIsActive(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	Rf_error(_("not a symbol"));
    env = simple_as_environment(env);
    if (!env)
	Rf_error(_("not an environment"));
    Frame::Binding* binding = findVarLocInFrame(env, sym, nullptr);
    if (!binding)
	Rf_error(_("no binding for \"%s\""), Rf_EncodeChar(PRINTNAME(sym)));
    return Rboolean(binding->isActive());
}

Rboolean R_HasFancyBindings(SEXP rho)
{
    SEXP frame;

    for (frame = FRAME(rho); frame != nullptr; frame = CDR(frame))
	if (IS_ACTIVE_BINDING(frame) || BINDING_IS_LOCKED(frame))
	    return TRUE;
    return FALSE;
}

HIDDEN SEXP do_lockBnd(/*const*/ Expression* call, const BuiltInFunction* op, RObject* sym_, RObject* env_)
{
    SEXP sym, env;
    sym = sym_;
    env = env_;
    switch(op->variant()) {
    case 0:
	R_LockBinding(sym, env);
	break;
    case 1:
	R_unLockBinding(sym, env);
	break;
    default:
	Rf_error(_("unknown op"));
    }
    return nullptr;
}

HIDDEN SEXP do_bndIsLocked(/*const*/ Expression* call, const BuiltInFunction* op, RObject* sym_, RObject* env_)
{
    SEXP sym, env;
    sym = sym_;
    env = env_;
    return Rf_ScalarLogical(R_BindingIsLocked(sym, env));
}

HIDDEN SEXP do_mkActiveBnd(/*const*/ Expression* call, const BuiltInFunction* op, RObject* sym_, RObject* fun_, RObject* env_)
{
    SEXP sym, fun, env;
    sym = sym_;
    fun = fun_;
    env = env_;
    R_MakeActiveBinding(sym, fun, env);
    return nullptr;
}

HIDDEN SEXP do_bndIsActive(/*const*/ Expression* call, const BuiltInFunction* op, RObject* sym_, RObject* env_)
{
    SEXP sym, env;
    sym = sym_;
    env = env_;
    return Rf_ScalarLogical(R_BindingIsActive(sym, env));
}

Rboolean R_IsPackageEnv(SEXP rho)
{
    if (TYPEOF(rho) == ENVSXP) {
	SEXP name = Rf_getAttrib(rho, Symbols::NameSymbol);
	const char *packprefix = "package:";
	size_t pplen = strlen(packprefix);
	if(Rf_isString(name) && Rf_length(name) > 0 &&
	   ! strncmp(packprefix, R_CHAR(STRING_ELT(name, 0)), pplen)) /* ASCII */
	    return TRUE;
	else
	    return FALSE;
    }
    else
	return FALSE;
}

SEXP R_PackageEnvName(SEXP rho)
{
    if (TYPEOF(rho) == ENVSXP) {
	SEXP name = Rf_getAttrib(rho, Symbols::NameSymbol);
	const char *packprefix = "package:";
	size_t pplen = strlen(packprefix);
	if(Rf_isString(name) && Rf_length(name) > 0 &&
	   ! strncmp(packprefix, R_CHAR(STRING_ELT(name, 0)), pplen)) /* ASCII */
	    return name;
	else
	    return nullptr;
    }
    else
	return nullptr;
}

const StringVector* Environment::packageName() const
{
    RObject* ans = R_PackageEnvName(const_cast<Environment*>(this));
    return SEXP_downcast<const StringVector*>(ans);
}

SEXP R_FindPackageEnv(SEXP info)
{
    static Symbol* s_findPackageEnv = Symbol::obtain("findPackageEnv");
    Expression* expr = new Expression(s_findPackageEnv, { info });
    return expr->evaluate(Environment::global());
}

Environment* Environment::findPackage(const std::string& name)
{
    GCStackRoot<StringVector> pkgsv(asStringVector(name));
    RObject* ans = R_FindPackageEnv(pkgsv);
    return SEXP_downcast<Environment*>(ans);
}

Rboolean R_IsNamespaceEnv(SEXP rho)
{
    if (rho == R_BaseNamespace)
	return TRUE;
    else if (TYPEOF(rho) == ENVSXP) {
	SEXP info = Rf_findVarInFrame3(rho, R_NamespaceSymbol, TRUE);
	if (info != R_UnboundValue && TYPEOF(info) == ENVSXP) {
	    SEXP spec = Rf_findVarInFrame3(info, Rf_install("spec"), TRUE);
	    if (spec != R_UnboundValue &&
		TYPEOF(spec) == STRSXP && LENGTH(spec) > 0)
		return TRUE;
	    else
		return FALSE;
	}
	else return FALSE;
    }
    else return FALSE;
}

HIDDEN SEXP do_isNSEnv(/*const*/ Expression* call, const BuiltInFunction* op, RObject* ns_)
{
    return R_IsNamespaceEnv(ns_) ? Rf_mkTrue() : Rf_mkFalse();
}

SEXP R_NamespaceEnvSpec(SEXP rho)
{
    /* The namespace spec is a character vector that specifies the
       namespace.  The first element is the namespace name.  The
       second element, if present, is the namespace version.  Further
       elements may be added later. */
    if (rho == R_BaseNamespace)
	return R_BaseNamespaceName;
    else if (TYPEOF(rho) == ENVSXP) {
	SEXP info = Rf_findVarInFrame3(rho, R_NamespaceSymbol, TRUE);
	if (info != R_UnboundValue && TYPEOF(info) == ENVSXP) {
	    SEXP spec = Rf_findVarInFrame3(info, Rf_install("spec"), TRUE);
	    if (spec != R_UnboundValue &&
		TYPEOF(spec) == STRSXP && LENGTH(spec) > 0)
		return spec;
	    else
		return nullptr;
	}
	else return nullptr;
    }
    else return nullptr;
}

const StringVector* Environment::namespaceSpec() const
{
    RObject* ans = R_NamespaceEnvSpec(const_cast<Environment*>(this));
    return SEXP_downcast<const StringVector*>(ans);
}

SEXP R_FindNamespace(SEXP info)
{
    SEXP expr, val;
    PROTECT(info);
    static const Symbol* s_getNamespace = Symbol::obtain("getNamespace");
    PROTECT(expr = Rf_lang2(const_cast<Symbol*>(s_getNamespace), info));
    val = Rf_eval(expr, R_GlobalEnv);
    UNPROTECT(2);
    return val;
}

Environment* Environment::findNamespace(const StringVector* spec)
{
    RObject* ans = R_FindNamespace(const_cast<StringVector*>(spec));
    return SEXP_downcast<Environment*>(ans);
}

static rho::Symbol* checkNSname(SEXP call, SEXP name)
{
    switch (TYPEOF(name)) {
    case SYMSXP:
	break;
    case STRSXP:
	if (LENGTH(name) >= 1) {
	    name = Rf_installTrChar(STRING_ELT(name, 0));
	    break;
	}
	/* else fall through */
    default:
	Rf_errorcall(call, _("bad namespace name"));
    }
    return SEXP_downcast<Symbol*>(name);
}

HIDDEN SEXP do_regNS(/*const*/ Expression* call, const BuiltInFunction* op, RObject* name_, RObject* env_)
{
    SEXP name, val;
    name = checkNSname(call, name_);
    val = env_;
    if (Rf_findVarInFrame(R_NamespaceRegistry, name) != R_UnboundValue)
	Rf_errorcall(call, _("namespace already registered"));
    Rf_defineVar(name, val, R_NamespaceRegistry);
    return nullptr;
}

HIDDEN SEXP do_unregNS(/*const*/ Expression* call, const BuiltInFunction* op, RObject* nsname_)
{
    rho::Symbol* name;
    name = checkNSname(call, nsname_);
    if (Rf_findVarInFrame(R_NamespaceRegistry, name) == R_UnboundValue)
	Rf_errorcall(call, _("namespace not registered"));
    RemoveVariable(name, SEXP_downcast<Environment*>(R_NamespaceRegistry));
    return nullptr;
}

HIDDEN SEXP do_getRegNS(/*const*/ Expression* call, const BuiltInFunction* op, RObject* name_)
{
    SEXP name, val;
    name = checkNSname(call, Rf_coerceVector(name_, SYMSXP));
    val = Rf_findVarInFrame(R_NamespaceRegistry, name);

    switch(op->variant()) {
    case 0: // get..()
	if (val == R_UnboundValue)
	    return nullptr;
	else
	    return val;
    case 1: // is..()
	return Rf_ScalarLogical(val == R_UnboundValue ? FALSE : TRUE);

    default: Rf_error(_("unknown op"));
    }
    return nullptr; // -Wall
}

HIDDEN SEXP do_getNSRegistry(/*const*/ Expression* call, const BuiltInFunction* op)
{
    return R_NamespaceRegistry;
}

HIDDEN SEXP do_importIntoEnv(/*const*/ Expression* call, const BuiltInFunction* op, RObject* impenv_, RObject* impnames_, RObject* expenv_, RObject* expnames_)
{
    /* This function copies values of variables from one environment
       to another environment, possibly with different names.
       Promises are not forced and active bindings are preserved. */
    SEXP impenv, impnames, expenv, expnames;
    SEXP impsym, expsym, val;
    int i, n;

    GCStackRoot<> binding;  // represented in PairList form.

    impenv = impenv_;
    impnames = impnames_;
    expenv = expenv_;
    expnames = expnames_;

    impenv = simple_as_environment(impenv);
    if (!impenv)
	Rf_error(_("bad import environment argument"));
    expenv = simple_as_environment(expenv);
    if (!expenv)
	Rf_error(_("bad import environment argument"));
    if (TYPEOF(impnames) != STRSXP || TYPEOF(expnames) != STRSXP)
	Rf_error(_("invalid '%s' argument"), "names");
    if (LENGTH(impnames) != LENGTH(expnames))
	Rf_error(_("length of import and export names must match"));

    n = LENGTH(impnames);
    for (i = 0; i < n; i++) {
	impsym = Rf_installTrChar(STRING_ELT(impnames, i));
	expsym = Rf_installTrChar(STRING_ELT(expnames, i));

	/* find the binding--may be a CONS cell or a symbol */
	SEXP binding = nullptr;
	for (SEXP env = expenv;
	     env != R_EmptyEnv && binding == nullptr;
	     env = ENCLOS(env)) {
	    Frame::Binding* bdg = findVarLocInFrame(env, expsym, nullptr);
	    binding = (bdg ? bdg->asPairList() : nullptr);
	}
	if (binding == nullptr)
	    binding = expsym;

	/* get value of the binding; do not force promises */
	if (TYPEOF(binding) == SYMSXP) {
	    if (SYMVALUE(expsym) == R_UnboundValue)
		Rf_error(_("exported symbol '%s' has no value"),
		      R_CHAR(PRINTNAME(expsym)));
	    val = SYMVALUE(expsym);
	}
	else val = CAR(binding);

	/* import the binding */
	if (IS_ACTIVE_BINDING(binding))
	    R_MakeActiveBinding(impsym, val, impenv);
	else Rf_defineVar(impsym, val, impenv);
    }
    return nullptr;
}


HIDDEN SEXP do_envprofile(/*const*/ Expression* call, const BuiltInFunction* op, RObject* env_)
{
    /* Return a list containing profiling information given a hashed
       environment.  For non-hashed environments, this function
       returns nullptr.  This seems appropriate since there is no
       way to test whether an environment is hashed at the R level.
    */
    // Unimplemented in rho:
    return nullptr;
}

// topenv
SEXP Rf_topenv(SEXP target, SEXP envir) {
    SEXP env = envir;
    while (env != R_EmptyEnv) {
	if (env == target || env == R_GlobalEnv ||
	    env == R_BaseEnv || env == R_BaseNamespace ||
	    R_IsPackageEnv(env) || R_IsNamespaceEnv(env) ||
	    findVarLocInFrame(envir, Symbols::DotPackageName, nullptr) != nullptr)
	{
	    return env;
	} else {
	    env = ENCLOS(env);
	}
    }
    return R_GlobalEnv;
}

/** topenv():
 *
 * .Internal(topenv(envir, matchThisEnv))
 *
 * @return
 */
HIDDEN SEXP do_topenv(SEXP call, SEXP op, SEXP args, SEXP rho) {
    SEXP envir = CAR(args);
    SEXP target = CADR(args); // = matchThisEnv, typically NULL (nullptr)
    if (TYPEOF(envir) != ENVSXP) envir = rho; // envir = parent.frame()
    if (target != nullptr && TYPEOF(target) != ENVSXP)  target = nullptr;
    return Rf_topenv(target, envir);
}

HIDDEN Rboolean Rf_isUnmodifiedSpecSym(SEXP sym, SEXP rho) {
    Symbol* symbol = SEXP_downcast<Symbol*>(sym);
    Environment* env = SEXP_downcast<Environment*>(rho);

    if (!symbol->isSpecialSymbol())
 	return FALSE;
    for(;env != R_EmptyEnv; env = env->enclosingEnvironment())
 	if (env != R_BaseEnv && env != R_BaseNamespace
	    && env->frame()->binding(symbol))
 	    return FALSE;
    return TRUE;
}

void findFunctionForBodyInNamespace(SEXP body, SEXP nsenv, SEXP nsname) {
    if (R_IsNamespaceEnv(nsenv) != TRUE)
	Rf_error("argument 'nsenv' is not a namespace");
    SEXP args = PROTECT(Rf_list3(nsenv /* x */,
	R_TrueValue /* all.names */,
	R_FalseValue /* sorted */));
    SEXP env2listOp = INTERNAL(Rf_install("env2list"));
///*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* all_names_, RObject* sorted_
    SEXP elist = do_env2list(nullptr, SEXP_downcast<BuiltInFunction*>(env2listOp), nsenv, R_TrueValue, R_FalseValue);
    PROTECT(elist);
    R_xlen_t n = Rf_xlength(elist);
    R_xlen_t i;
    SEXP names = PROTECT(Rf_getAttrib(elist, Symbols::NamesSymbol));
    for(i = 0; i < n; i++) {
	SEXP value = VECTOR_ELT(elist, i);
	const char *vname = R_CHAR(STRING_ELT(names, i));
	/* the constants checking requires shallow comparison */
	if (TYPEOF(value) == CLOSXP && R_ClosureExpr(value) == body)
	    REprintf("Function %s in namespace %s has this body.\n",
		vname,
		R_CHAR(PRINTNAME(nsname)));
	/* search S4 registry */
	const char *s4prefix = ".__T__";
	if (TYPEOF(value) == ENVSXP &&
		streqln(vname, s4prefix, strlen(s4prefix))) {
	    SETCAR(args, value); /* re-use args */
	    SEXP rlist = do_env2list(nullptr, SEXP_downcast<BuiltInFunction*>(env2listOp), nsenv, R_TrueValue, R_FalseValue);
	    PROTECT(rlist);
	    R_xlen_t rn = Rf_xlength(rlist);
	    R_xlen_t ri;
	    SEXP rnames = PROTECT(Rf_getAttrib(rlist, Symbols::NamesSymbol));
	    for(ri = 0; ri < rn; ri++) {
		SEXP rvalue = VECTOR_ELT(rlist, ri);
		/* the constants checking requires shallow comparison */
		if (TYPEOF(rvalue) == CLOSXP &&
			R_ClosureExpr(rvalue) == body)
		    REprintf("S4 Method %s defined in namespace %s with signature %s has this body.\n",
			vname + strlen(s4prefix),
			R_CHAR(PRINTNAME(nsname)),
			R_CHAR(STRING_ELT(rnames, ri)));
	    }
	    UNPROTECT(2); /* rlist, rnames */
	}
    }
    UNPROTECT(3); /* names, elist, args */
}

/*  findFunctionForBody - for a given function body, try to find a closure and
    the name of its binding (and the name of the package). For debugging. */
HIDDEN void Rf_findFunctionForBody(SEXP body) {
    SEXP nstable = HASHTAB(R_NamespaceRegistry);
    //CHECK_HASH_TABLE(nstable);
    int n = Rf_length(nstable);
    int i;
    for(i = 0; i < n; i++) {
	SEXP frame = VECTOR_ELT(nstable, i);
	while (frame) {
	    findFunctionForBodyInNamespace(body, CAR(frame), TAG(frame));
	    frame = CDR(frame);
	}
    }
}

