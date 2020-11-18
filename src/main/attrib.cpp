/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997--2017  The R Core Team
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
 */

#define R_NO_REMAP

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Localization.h>
#include <Internal.h>
#include <Rmath.h>
#include "basedecl.h"
#include <rho/ArgMatcher.hpp>
#include <rho/GCStackRoot.hpp>

using namespace rho;

#undef TRUE
#undef FALSE

static RObject* removeAttrib(RObject* vec, Symbol* name);

SEXP comment(SEXP);
static RObject* commentgets(RObject* vec, RObject* comment);

static RObject* row_names_gets(RObject* vec, RObject* val)
{
    RObject* ans;

    if (vec == nullptr)
	Rf_error(_("attempt to set an attribute on NULL"));

    if(Rf_isReal(val) && LENGTH(val) == 2 && std::isnan(REAL(val)[0]) ) {
	/* This should not happen, but if a careless user dput()s a
	   data frame and sources the result, it will */
	PROTECT(val = Rf_coerceVector(val, INTSXP));
	vec->setAttribute(Symbols::RowNamesSymbol, val);
	ans = val;
	UNPROTECT(1);
	return ans;
    }
    if(Rf_isInteger(val)) {
	bool OK_compact = true;
	int i, n = LENGTH(val);
	if(n == 2 && INTEGER(val)[0] == R_NaInt) {
	    n = INTEGER(val)[1];
	} else if (n > 2) {
	    for(i = 0; i < n; i++)
		if(INTEGER(val)[i] != i+1) {
		    OK_compact = false;
		    break;
		}
	} else OK_compact = false;
	if(OK_compact) {
	    /* we hide the length in an impossible integer vector */
	    PROTECT(val = Rf_allocVector(INTSXP, 2));
	    INTEGER(val)[0] = R_NaInt;
	    INTEGER(val)[1] = n;
	    vec->setAttribute(Symbols::RowNamesSymbol, val);
	    ans = val;
	    UNPROTECT(1);
	    return ans;
	}
    } else if(!Rf_isString(val))
	Rf_error(_("row names must be 'character' or 'integer', not '%s'"),
	      Rf_type2char(TYPEOF(val)));
    PROTECT(val);
    vec->setAttribute(Symbols::RowNamesSymbol, val);
    ans =  val;
    UNPROTECT(1);
    return ans;
}


static bool isOneDimensionalArray(SEXP vec)
{
    if(Rf_isVector(vec) || Rf_isList(vec) || Rf_isLanguage(vec)) {
	SEXP s = Rf_getAttrib(vec, Symbols::DimSymbol);
	if(TYPEOF(s) == INTSXP && LENGTH(s) == 1)
	    return true;
    }
    return false;
}

/* NOTE: For environments serialize.cpp calls this function to find if
   there is a class attribute in order to reconstruct the object bit
   if needed.  This means the function cannot use OBJECT(vec) == 0 to
   conclude that the class attribute is nullptr.  If you want to
   rewrite this function to use such a pre-test, be sure to adjust
   serialize.cpp accordingly.  LT */
HIDDEN SEXP getAttrib0(SEXP vec, SEXP name)
{
    SEXP s;
    int len, i, any;

    if (!vec) return nullptr;
    if (name == Symbols::NamesSymbol) {
	if(isOneDimensionalArray(vec)) {
	    s = Rf_getAttrib(vec, Symbols::DimNamesSymbol);
	    if(!Rf_isNull(s)) {
		MARK_NOT_MUTABLE(VECTOR_ELT(s, 0));
		return VECTOR_ELT(s, 0);
	    }
	}
	if (Rf_isList(vec) || Rf_isLanguage(vec)) {
	    len = Rf_length(vec);
	    PROTECT(s = Rf_allocVector(STRSXP, len));
	    i = 0;
	    any = 0;
	    for ( ; vec != nullptr; vec = CDR(vec), i++) {
		if (TAG(vec) == nullptr)
		    SET_STRING_ELT(s, i, R_BlankString);
		else if (Rf_isSymbol(TAG(vec))) {
		    any = 1;
		    SET_STRING_ELT(s, i, PRINTNAME(TAG(vec)));
		}
		else
		    Rf_error(_("getAttrib: invalid type (%s) for TAG"),
			  Rf_type2char(TYPEOF(TAG(vec))));
	    }
	    UNPROTECT(1);
	    if (any) {
		if (!Rf_isNull(s)) MARK_NOT_MUTABLE(s);
		return (s);
	    }
	    return nullptr;
	}
    }
    /* This is where the old/new list adjustment happens. */
    RObject* att = vec->getAttribute(SEXP_downcast<Symbol*>(name));
    if (!att) return nullptr;
    if (name == Symbols::DimNamesSymbol && TYPEOF(att) == LISTSXP)
	Rf_error("old list is no longer allowed for dimnames attribute\n");
	    /**** this could be dropped for REFCNT or be less
		  stringent for NAMED for attributes where the setter
		  does not have a consistency check that could fail
		  after mutation in a complex assignment LT */
    MARK_NOT_MUTABLE(att);
    return att;
}

SEXP Rf_getAttrib(SEXP vec, SEXP name)
{
    if (!vec) return nullptr;
    if(TYPEOF(vec) == CHARSXP)
	Rf_error("cannot have attributes on a CHARSXP");
    /* pre-test to avoid expensive operations if clearly not needed -- LT */
    if (!vec->hasAttributes() &&
	! (TYPEOF(vec) == LISTSXP || TYPEOF(vec) == LANGSXP))
	return nullptr;

    if (Rf_isString(name)) name = Rf_installTrChar(STRING_ELT(name, 0));

    /* special test for c(NA, n) rownames of data frames: */
    if (name == Symbols::RowNamesSymbol) {
	SEXP s = getAttrib0(vec, Symbols::RowNamesSymbol);
	if(Rf_isInteger(s) && LENGTH(s) == 2 && INTEGER(s)[0] == R_NaInt) {
	    int n = abs(INTEGER(s)[1]);
	    if (n > 0) {
		//  s = R_compact_intrange(1, n);
		s = Rf_allocVector(INTSXP, n);
		for(int i = 0; i < n; i++)
		INTEGER(s)[i] = i+1;
	    } else
		s = Rf_allocVector(INTSXP, 0);
	}
	return s;
    } else
	return getAttrib0(vec, name);
}

// R's .row_names_info(x, type = 1L) := .Internal(shortRowNames(x, type)) :
HIDDEN
SEXP do_shortRowNames(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* type_)
{
    /* return  n if the data frame 'vec' has c(NA, n) rownames;
     *	       nrow(.) otherwise;  note that data frames with nrow(.) == 0
     *		have no row.names.
     ==> is also used in dim.data.frame() */

    SEXP s = getAttrib0(x_, Symbols::RowNamesSymbol), ans = s;
    int type = Rf_asInteger(type_);

    if( type < 0 || type > 2)
	Rf_error(_("invalid '%s' argument"), "type");

    if(type >= 1) {
	int n = (Rf_isInteger(s) && LENGTH(s) == 2 && INTEGER(s)[0] == R_NaInt)
	    ? INTEGER(s)[1] : (Rf_isNull(s) ? 0 : LENGTH(s));
	ans = Rf_ScalarInteger((type == 1) ? n : abs(n));
    }
    return ans;
}

/* This is allowed to change 'out' */
HIDDEN
SEXP do_copyDFattr(/*const*/ Expression* call, const BuiltInFunction* op, RObject* xx_, RObject* x_)
{
    SEXP in = xx_, out = x_;
    SET_ATTRIB(out, Rf_shallow_duplicate(ATTRIB(in)));
    IS_S4_OBJECT(in) ?  SET_S4_OBJECT(out) : UNSET_S4_OBJECT(out);
    return out;
}


/* 'name' should be 1-element STRSXP or SYMSXP */
SEXP Rf_setAttrib(SEXP vec, SEXP name, SEXP val)
{
    PROTECT(vec);
    PROTECT(name);

    if (Rf_isString(name)) {
	name = Rf_installTrChar(STRING_ELT(name, 0));
    }
    if (val == nullptr) {
	/* FIXME: see do_namesgets().
	if (name == Symbols::NamesSymbol && isOneDimensionalArray(vec)) {
	    UNPROTECT(2);
	    return removeAttrib(vec, Symbols::DimNamesSymbol);
	}
	*/
	UNPROTECT(2);
	return removeAttrib(vec, SEXP_downcast<Symbol*>(name));
    }

    /* We allow attempting to remove names from NULL */
    if (vec == nullptr)
	Rf_error(_("attempt to set an attribute on NULL"));

    if (MAYBE_REFERENCED(val)) val = R_FixupRHS(vec, val);
    UNPROTECT(2);

    GCStackRoot<> valr(val);
    if (name == Symbols::NamesSymbol)
	return Rf_namesgets(vec, val);
    else if (name == Symbols::DimSymbol)
	return Rf_dimgets(vec, val);
    else if (name == Symbols::DimNamesSymbol)
	return Rf_dimnamesgets(vec, val);
    else if (name == Symbols::ClassSymbol)
	return Rf_classgets(vec, val);
    else if (name == Symbols::TspSymbol)
	return Rf_tspgets(vec, val);
    else if (name == Symbols::CommentSymbol)
	return commentgets(vec, val);
    else if (name == Symbols::RowNamesSymbol) // "row.names" -> care for data frames
	return row_names_gets(vec, val);
    else {
	vec->setAttribute(SEXP_downcast<Symbol*>(name), val);
	return val;
    }
}

/* This is called in the case of binary operations to copy */
/* most attributes from (one of) the input arguments to */
/* the output.	Note that the Dim and Names attributes */
/* should have been assigned elsewhere. */

void Rf_copyMostAttrib(SEXP inp, SEXP ans)
{
    SEXP s;

    if (ans == nullptr)
	Rf_error(_("attempt to set an attribute on NULL"));

    PROTECT(ans);
    PROTECT(inp);
    for (s = ATTRIB(inp); s != nullptr; s = CDR(s)) {
	if ((TAG(s) != Symbols::NamesSymbol) &&
	    (TAG(s) != Symbols::DimSymbol) &&
	    (TAG(s) != Symbols::DimNamesSymbol)) {
	    ans->setAttribute(SEXP_downcast<Symbol*>(TAG(s)), CAR(s));
	}
    }
    IS_S4_OBJECT(inp) ?  SET_S4_OBJECT(ans) : UNSET_S4_OBJECT(ans);
    UNPROTECT(2);
}

/* version that does not preserve ts information, for subsetting */
void Rf_copyMostAttribNoTs(SEXP inp, SEXP ans)
{
    SEXP s;

    if (ans == nullptr)
	Rf_error(_("attempt to set an attribute on NULL"));

    PROTECT(ans);
    PROTECT(inp);
    for (s = ATTRIB(inp); s != nullptr; s = CDR(s)) {
	if ((TAG(s) != Symbols::NamesSymbol) &&
	    (TAG(s) != Symbols::ClassSymbol) &&
	    (TAG(s) != Symbols::TspSymbol) &&
	    (TAG(s) != Symbols::DimSymbol) &&
	    (TAG(s) != Symbols::DimNamesSymbol)) {
	    ans->setAttribute(SEXP_downcast<Symbol*>(TAG(s)), CAR(s));
	} else if (TAG(s) == Symbols::ClassSymbol) {
	    SEXP cl = CAR(s);
	    int i;
	    Rboolean ists = FALSE;
	    for (i = 0; i < LENGTH(cl); i++)
		if (streql(R_CHAR(STRING_ELT(cl, i)), "ts")) { /* ASCII */
		    ists = TRUE;
		    break;
		}
	    if (!ists) ans->setAttribute(SEXP_downcast<Symbol*>(TAG(s)), cl);
	    else if(LENGTH(cl) <= 1) {
	    } else {
		SEXP new_cl;
		int i, j, l = LENGTH(cl);
		PROTECT(new_cl = Rf_allocVector(STRSXP, l - 1));
		for (i = 0, j = 0; i < l; i++)
		    if (strcmp(R_CHAR(STRING_ELT(cl, i)), "ts")) /* ASCII */
			SET_STRING_ELT(new_cl, j++, STRING_ELT(cl, i));
		ans->setAttribute(SEXP_downcast<Symbol*>(TAG(s)), new_cl);
		UNPROTECT(1);
	    }
	}
    }
    IS_S4_OBJECT(inp) ?  SET_S4_OBJECT(ans) : UNSET_S4_OBJECT(ans);
    UNPROTECT(2);
}

static RObject* removeAttrib(RObject* vec, Symbol* name)
{
    if (!vec) return nullptr;  // 2007/07/24 arr
    if(TYPEOF(vec) == CHARSXP)
	Rf_error("cannot set attribute on a CHARSXP");
    if (name == Symbols::NamesSymbol && Rf_isPairList(vec)) {
	for (RObject* t = vec; t != nullptr; t = CDR(t))
	    SET_TAG(t, nullptr);
	return nullptr;
    }
    else {
	if (name == Symbols::DimSymbol)
	    vec->setAttribute(Symbols::DimNamesSymbol, nullptr);
	vec->setAttribute(SEXP_downcast<Symbol*>(name), nullptr);
    }
    return nullptr;
}

static void checkNames(RObject* x, RObject* s)
{
    if (Rf_isVector(x) || Rf_isList(x) || Rf_isLanguage(x)) {
	if (!Rf_isVector(s) && !Rf_isList(s))
	    Rf_error(_("invalid type (%s) for 'names': must be vector"),
		  Rf_type2char(TYPEOF(s)));
	if (Rf_xlength(x) != Rf_xlength(s))
	    Rf_error(_("'names' attribute [%d] must be the same length as the vector [%d]"), Rf_length(s), Rf_length(x));
    }
    else if(IS_S4_OBJECT(x)) {
      /* leave validity checks to S4 code */
    }
    else Rf_error(_("names() applied to a non-vector"));
}


/* Time Series Parameters */

[[noreturn]] static void badtsp(void)
{
    Rf_error(_("invalid time series parameters specified"));
}

HIDDEN
SEXP Rf_tspgets(SEXP vec, SEXP val)
{
    double start, end, frequency;
    int n;

    if (vec == nullptr)
	Rf_error(_("attempt to set an attribute on NULL"));

    if(IS_S4_OBJECT(vec)) { /* leave validity checking to validObject */
	if (!Rf_isNumeric(val)) /* but should have been checked */
	    Rf_error(_("'tsp' attribute must be numeric"));
	vec->setAttribute(Symbols::TspSymbol, val);
	return vec;
    }

    if (!Rf_isNumeric(val) || LENGTH(val) != 3)
	Rf_error(_("'tsp' attribute must be numeric of length three"));

    if (Rf_isReal(val)) {
	start = REAL(val)[0];
	end = REAL(val)[1];
	frequency = REAL(val)[2];
    }
    else {
	start = (INTEGER(val)[0] == R_NaInt) ?
	    R_NaReal : INTEGER(val)[0];
	end = (INTEGER(val)[1] == R_NaInt) ?
	    R_NaReal : INTEGER(val)[1];
	frequency = (INTEGER(val)[2] == R_NaInt) ?
	    R_NaReal : INTEGER(val)[2];
    }
    if (frequency <= 0) badtsp();
    n = Rf_nrows(vec);
    if (n == 0) Rf_error(_("cannot assign 'tsp' to zero-length vector"));

    /* FIXME:  1.e-5 should rather be == option('ts.eps') !! */
    if (std::abs(end - start - (n - 1)/frequency) > 1.e-5)
	badtsp();

    PROTECT(vec);
    val = Rf_allocVector(REALSXP, 3);
    PROTECT(val);
    REAL(val)[0] = start;
    REAL(val)[1] = end;
    REAL(val)[2] = frequency;
    vec->setAttribute(Symbols::TspSymbol, val);
    UNPROTECT(2);
    return vec;
}

static RObject* commentgets(RObject* vec, RObject* comment)
{
    if (vec == nullptr)
	Rf_error(_("attempt to set an attribute on NULL"));

    if (Rf_isNull(comment) || Rf_isString(comment)) {
	vec->setAttribute(Symbols::CommentSymbol,
			  Rf_length(comment) <= 0 ? nullptr : comment);
	return nullptr;
    }
    Rf_error(_("attempt to set invalid 'comment' attribute"));
    return nullptr;/*- just for -Wall */
}

HIDDEN SEXP do_commentgets(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* value_)
{
    RObject* object = x_;
    RObject* comment = value_;
    if (MAYBE_SHARED(object)) object = Rf_duplicate(object);
    if (Rf_length(comment) == 0) comment = nullptr;
    Rf_setAttrib(object, Symbols::CommentSymbol, comment);
    SETTER_CLEAR_NAMED(object);
    return object;
}

HIDDEN SEXP do_comment(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_)
{
    return Rf_getAttrib(x_, Symbols::CommentSymbol);
}

SEXP Rf_classgets(SEXP vec, SEXP klass)
{
    if (Rf_isNull(klass) || Rf_isString(klass)) {
	int ncl = Rf_length(klass);
  	if (ncl <= 0) {
	    vec->setAttribute(Symbols::ClassSymbol, nullptr);
	    // problems when package building:  UNSET_S4_OBJECT(vec);
	}
	else {
	    /* When data frames were a special data type */
	    /* we had more exhaustive checks here.  Now that */
	    /* use JMCs interpreted code, we don't need this */
	    /* FIXME : The whole "classgets" may as well die. */

	    /* HOWEVER, it is the way that the object bit gets set/unset */

	    Rboolean isfactor = FALSE;

	    if (vec == nullptr)
		Rf_error(_("attempt to set an attribute on NULL"));

	    for(int i = 0; i < ncl; i++)
		if(streql(R_CHAR(STRING_ELT(klass, i)), "factor")) { /* ASCII */
		    isfactor = TRUE;
		    break;
		}
	    if(isfactor && TYPEOF(vec) != INTSXP) {
		/* we cannot coerce vec here, so just fail */
		Rf_error(_("adding class \"factor\" to an invalid object"));
	    }

	    vec->setAttribute(Symbols::ClassSymbol, klass);

#ifdef R_classgets_copy_S4
// not ok -- fails at installation around byte-compiling methods
	    if(ncl == 1 && R_has_methods_attached()) { // methods: do not act too early
		SEXP cld = R_getClassDef_R(klass);
		if(!Rf_isNull(cld)) {
		    PROTECT(cld);
		    /* More efficient? can we protect? -- rather *assign* in method-ns?
		       static SEXP oldCl = nullptr;
		       if(!oldCl) oldCl = R_getClassDef("oldClass");
		       if(!oldCl) oldCl = Rf_mkString("oldClass");
		       PROTECT(oldCl);
		    */
		    if(!R_isVirtualClass(cld, R_MethodsNamespace) &&
		       !R_extends(cld, Rf_mkString("oldClass"), R_MethodsNamespace)) // set S4 bit :
			// !R_extends(cld, oldCl, R_MethodsNamespace)) // set S4 bit :

			SET_S4_OBJECT(vec);

		    UNPROTECT(1); // UNPROTECT(2);
		}
	    }
#endif
	}
	return nullptr;
    }
    Rf_error(_("attempt to set invalid 'class' attribute"));
    return nullptr;/*- just for -Wall */
}

/* oldClass<-(), primitive */
HIDDEN SEXP do_classgets(/*const*/ Expression* call, const BuiltInFunction* op, RObject* object, RObject* new_class)
{
    if (MAYBE_SHARED(object)) object = Rf_shallow_duplicate(object);
    if (Rf_length(new_class) == 0) new_class = nullptr;
    if(IS_S4_OBJECT(object))
	UNSET_S4_OBJECT(object);
    Rf_setAttrib(object, Symbols::ClassSymbol, new_class);
    SETTER_CLEAR_NAMED(object);
    return object;
}

/* oldClass, primitive */
HIDDEN SEXP do_class(Expression* call, const BuiltInFunction* op, RObject* x)
{
    SEXP s3class;
    if(IS_S4_OBJECT(x)) {
      if((s3class = Rf_S3Class(x)) != nullptr) {
	return s3class;
      }
    } /* else */
    return Rf_getAttrib(x, Symbols::ClassSymbol);
}

/* character elements corresponding to the syntactic types in the
   grammar */
static RObject* lang2str(RObject* obj, SEXPTYPE t)
{
  SEXP symb = CAR(obj);
  static GCRoot<> if_sym = nullptr, while_sym, for_sym, eq_sym, gets_sym,
    lpar_sym, lbrace_sym, call_sym;
  if(!if_sym) {
    /* initialize:  another place for a hash table */
    if_sym = Rf_install("if");
    while_sym = Rf_install("while");
    for_sym = Rf_install("for");
    eq_sym = Rf_install("=");
    gets_sym = Rf_install("<-");
    lpar_sym = Rf_install("(");
    lbrace_sym = Rf_install("{");
    call_sym = Rf_install("call");
  }
  if(Rf_isSymbol(symb)) {
    if(symb == if_sym || symb == for_sym || symb == while_sym ||
       symb == lpar_sym || symb == lbrace_sym ||
       symb == eq_sym || symb == gets_sym)
      return PRINTNAME(symb);
  }
  return PRINTNAME(call_sym);
}

/* the S4-style class: for dispatch required to be a single string;
   for the new class() function;
   if(!singleString) , keeps S3-style multiple classes.
   Called from the methods package, so exposed.
 */
SEXP R_data_class(SEXP obj, Rboolean singleString)
{
    SEXP value, klass = Rf_getAttrib(obj, Symbols::ClassSymbol);
    int n = Rf_length(klass);
    if(n == 1 || (n > 0 && !singleString))
	return(klass);
    if(n == 0) {
	SEXP dim = Rf_getAttrib(obj, Symbols::DimSymbol);
	int nd = Rf_length(dim);
	if(nd > 0) {
	    if(nd == 2)
		klass = Rf_mkChar("matrix");
	    else
		klass = Rf_mkChar("array");
	}
	else {
	  SEXPTYPE t = TYPEOF(obj);
	  switch(t) {
	  case CLOSXP: case SPECIALSXP: case BUILTINSXP:
	    klass = Rf_mkChar("function");
	    break;
	  case REALSXP:
	    klass = Rf_mkChar("numeric");
	    break;
	  case SYMSXP:
	    klass = Rf_mkChar("name");
	    break;
	  case LANGSXP:
	    klass = lang2str(obj, t);
	    break;
	  default:
	    klass = Rf_type2str(t);
	  }
	}
    }
    else
	klass = Rf_asChar(klass);
    PROTECT(klass);
    value = Rf_ScalarString(klass);
    UNPROTECT(1);
    return value;
}

static GCRoot<> s_dot_S3Class = nullptr;

static GCRoot<> R_S4_extends_table = nullptr;

static RObject* cache_class(const char *class_str, RObject* klass)
{
    if(!R_S4_extends_table) {
	R_S4_extends_table = R_NewHashedEnv(nullptr, Rf_ScalarInteger(0));
	R_PreserveObject(R_S4_extends_table);
    }
    if(Rf_isNull(klass)) { /* retrieve cached value */
	RObject* val = Rf_findVarInFrame(R_S4_extends_table, rho::Symbol::obtain(class_str));
	return (val == R_UnboundValue) ? klass : val;
    }
    Rf_defineVar(rho::Symbol::obtain(class_str), klass, R_S4_extends_table);
    return klass;
}

static RObject* S4_extends(RObject* klass, bool use_tab) {
    static Symbol* s_extends = nullptr, *s_extendsForS3 = nullptr;
    RObject* e;
    RObject* val; const char *class_str;
    const void *vmax = nullptr;
    if(use_tab) vmax = vmaxget();
    if(!s_extends) {
	s_extends= Symbol::obtain("extends");
	s_extendsForS3 = Symbol::obtain(".extendsForS3");
	R_S4_extends_table = R_NewHashedEnv(nullptr, Rf_ScalarInteger(0));
    }
    if(!isMethodsDispatchOn()) {
        return klass;
    }
    class_str = Rf_translateChar(STRING_ELT(klass, 0)); /* TODO: include package attr. */
    if(use_tab) {
	val = Rf_findVarInFrame(R_S4_extends_table, rho::Symbol::obtain(class_str));
	vmaxset(vmax);
	if(val != R_UnboundValue)
	    return val;
    }
    // else:  val <- .extendsForS3(klass) -- and cache it
    PROTECT(e = Rf_allocVector(LANGSXP, 2));
    SETCAR(e, s_extendsForS3);
    val = CDR(e);
    SETCAR(val, klass);
    val = Rf_eval(e, R_MethodsNamespace);
    cache_class(class_str, val);
    UNPROTECT(1);
    return(val);
}

SEXP R_S4_extends(SEXP klass, SEXP useTable)
{
    return S4_extends(klass, Rf_asLogical(useTable));
}


/* pre-allocated default class attributes */
static struct Type2DefaultClass {
    SEXP vector;
    SEXP matrix;
    SEXP array;
} Type2DefaultClass[MAX_NUM_SEXPTYPE];


static RObject* createDefaultClass(RObject* part1, RObject* part2, RObject* part3)
{
    int size = 0;
    if (part1 != nullptr) size++;
    if (part2 != nullptr) size++;
    if (part3 != nullptr) size++;

    if (size == 0 || part2 == nullptr) return nullptr;

    SEXP res = Rf_allocVector(STRSXP, size);
    R_PreserveObject(res);

    int i = 0;
    if (part1 != nullptr) SET_STRING_ELT(res, i++, part1);
    if (part2 != nullptr) SET_STRING_ELT(res, i++, part2);
    if (part3 != nullptr) SET_STRING_ELT(res, i, part3);

    MARK_NOT_MUTABLE(res);
    return res;
}

HIDDEN
void Rf_InitS3DefaultTypes()
{
    for(int type = 0; type < MAX_NUM_SEXPTYPE; type++) {
	SEXP part2 = nullptr;
	SEXP part3 = nullptr;
	int nprotected = 0;

	switch(type) {
	    case CLOSXP:
	    case SPECIALSXP:
	    case BUILTINSXP:
		part2 = PROTECT(Rf_mkChar("function"));
		nprotected++;
		break;
	    case INTSXP:
	    case REALSXP:
	        part2 = PROTECT(Rf_type2str_nowarn(SEXPTYPE(type)));
		part3 = PROTECT(Rf_mkChar("numeric"));
		nprotected += 2;
		break;
	    case LANGSXP:
		/* part2 remains nullptr: default type cannot be
		   pre-allocated, as it depends on the object value */
		break;
	    case SYMSXP:
		part2 = PROTECT(Rf_mkChar("name"));
		nprotected++;
		break;
	    default:
	        part2 = PROTECT(Rf_type2str_nowarn(SEXPTYPE(type)));
		nprotected++;
	}

	Type2DefaultClass[type].vector =
	    createDefaultClass(nullptr, part2, part3);
	Type2DefaultClass[type].matrix =
	    createDefaultClass(Rf_mkChar("matrix"), part2, part3);
	Type2DefaultClass[type].array =
	    createDefaultClass(Rf_mkChar("array"), part2, part3);
	UNPROTECT(nprotected);
    }
}

/* Version for S3-dispatch */
HIDDEN SEXP R_data_class2 (SEXP obj)
{
    SEXP klass = Rf_getAttrib(obj, Symbols::ClassSymbol);
    if(Rf_length(klass) > 0) {
	if(IS_S4_OBJECT(obj))
	    return S4_extends(klass, TRUE);
	else
	    return klass;
    }
    else { /* Rf_length(klass) == 0 */

	SEXP dim = Rf_getAttrib(obj, Symbols::DimSymbol);
	int n = Rf_length(dim);
	SEXPTYPE t = TYPEOF(obj);
	SEXP defaultClass;
	switch(n) {
	case 0:  defaultClass = Type2DefaultClass[t].vector; break;
	case 2:  defaultClass = Type2DefaultClass[t].matrix; break;
	default: defaultClass = Type2DefaultClass[t].array;  break;
	}

	if (defaultClass != nullptr) {
	    return defaultClass;
	}

	/* now t == LANGSXP, but check to make sure */
	if (t != LANGSXP)
	    Rf_error("type must be LANGSXP at this point");
	if (n == 0) {
	    return Rf_ScalarString(lang2str(obj, t));
	}
	SEXP part1;
	if (n == 2) {
	    part1 = Rf_mkChar("matrix");
	} else {
	    part1 = Rf_mkChar("array");
	}
	PROTECT(part1);
	defaultClass = PROTECT(Rf_allocVector(STRSXP, 2));
	SET_STRING_ELT(defaultClass, 0, part1);
	SET_STRING_ELT(defaultClass, 1, lang2str(obj, t));
	UNPROTECT(2); /* part1, defaultClass */
	return defaultClass;
    }
}

// class(x)  &  .cache_class(classname, extendsForS3(.)) {called from methods} :
HIDDEN SEXP R_do_cache_data_class(/*const*/ Expression* call, const BuiltInFunction* op, RObject* klass, RObject* value)
{
    call->check1arg("class");
    if(TYPEOF(klass) != STRSXP || LENGTH(klass) < 1)
	Rf_error("invalid class argument to internal .class_cache");
    const char *class_str = Rf_translateChar(STRING_ELT(klass, 0));
    return cache_class(class_str, value);
}

HIDDEN SEXP R_do_data_class(/*const*/ Expression* call, const BuiltInFunction* op, RObject* klass)
{
    call->check1arg("x");
    return R_data_class(klass, FALSE);
}

/* names(object) <- name */
HIDDEN SEXP do_namesgets(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* value_)
{
    RObject* object = x_;
    RObject* names = value_;
    /* Special case: removing non-existent names, to avoid a copy */
    if (names == nullptr &&
	Rf_getAttrib(object, Symbols::NamesSymbol) == nullptr)
	return object;

    if (MAYBE_SHARED(object))
	object = Rf_shallow_duplicate(object);

    if (TYPEOF(object) == S4SXP) {
	const char *klass = R_CHAR(STRING_ELT(R_data_class(object, FALSE), 0));
	Rf_error(_("invalid to use names()<- on an S4 object of class '%s'"),
	      klass);
    }
    if (names != nullptr &&
	! (TYPEOF(names) == STRSXP && ATTRIB(names) == nullptr)) {
	//GCStackRoot<PairList> tl(new PairList);
      PROTECT(call = new Expression(Symbols::AsCharacterSymbol, { names }));
	names = Rf_eval(call, R_BaseEnv);
	UNPROTECT(1);
    }
    /* FIXME:
       Need to special-case names(x) <- NULL for 1-d arrays to perform
         Rf_setAttrib(x, Symbols::DimNamesSymbol, nullptr)
       (and remove the dimnames) here if we want
         Rf_setAttrib(x, Symbols::NamesSymbol, nullptr)
       to actually remove the names, as needed in subset.cpp.
    */
    if(names == nullptr && isOneDimensionalArray(object))
	Rf_setAttrib(object, Symbols::DimNamesSymbol, names);
    else
	Rf_setAttrib(object, Symbols::NamesSymbol, names);
    UNPROTECT(1);
    SETTER_CLEAR_NAMED(object);
    return object;
}

SEXP Rf_namesgets(SEXP vec, SEXP val)
{
    int i;
    SEXP s, rval, tval;

    PROTECT(vec);
    PROTECT(val);

    /* Ensure that the labels are indeed */
    /* a vector of character strings */

    if (Rf_isList(val)) {
	if (!Rf_isVectorizable(val))
	    Rf_error(_("incompatible 'names' argument"));
	else {
	    rval = Rf_allocVector(STRSXP, Rf_length(vec));
	    PROTECT(rval);
	    /* See PR#10807 */
	    for (i = 0, tval = val;
		 i < Rf_length(vec) && tval != nullptr;
		 i++, tval = CDR(tval)) {
		s = Rf_coerceVector(CAR(tval), STRSXP);
		SET_STRING_ELT(rval, i, STRING_ELT(s, 0));
	    }
	    UNPROTECT(1);
	    val = rval;
	}
    } else val = Rf_coerceVector(val, STRSXP);
    UNPROTECT(1);
    PROTECT(val);

    /* Check that the lengths and types are compatible */

    if (Rf_xlength(val) < Rf_xlength(vec)) {
	val = Rf_xlengthgets(val, Rf_xlength(vec));
	UNPROTECT(1);
	PROTECT(val);
    }

    checkNames(vec, val);

    /* Special treatment for one dimensional arrays */
    if(isOneDimensionalArray(vec)) {
	PROTECT(val = PairList::cons(val, nullptr));
	Rf_setAttrib(vec, Symbols::DimNamesSymbol, val);
	UNPROTECT(3);
	return vec;
    }

    if (Rf_isList(vec) || Rf_isLanguage(vec)) {
	/* Cons-cell based objects */
	i = 0;
	for (s = vec; s != nullptr; s = CDR(s), i++)
	    if (STRING_ELT(val, i) != nullptr
		&& STRING_ELT(val, i) != R_NaString
		&& *R_CHAR(STRING_ELT(val, i)) != 0) /* test of length */
		SET_TAG(s, Rf_installTrChar(STRING_ELT(val, i)));
	    else
		SET_TAG(s, nullptr);
    }
    else if (Rf_isVector(vec) || IS_S4_OBJECT(vec))
	/* Normal case */
	vec->setAttribute(Symbols::NamesSymbol, val);
    else
	Rf_error(_("invalid type (%s) to set 'names' attribute"),
	      Rf_type2char(TYPEOF(vec)));
    UNPROTECT(2);
    return vec;
}

#define isS4Environment(x) (TYPEOF(x) == S4SXP &&	\
			    Rf_isEnvironment(R_getS4DataSlot(x, ENVSXP)))

HIDDEN SEXP do_names(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_)
{
    SEXP ans;
    ans = x_;
    if (Rf_isEnvironment(ans) || isS4Environment(ans))
	ans = R_lsInternal3(ans, TRUE, FALSE);
    else if (Rf_isVector(ans) || Rf_isList(ans) || Rf_isLanguage(ans) ||
	     IS_S4_OBJECT(ans))
	ans = Rf_getAttrib(ans, Symbols::NamesSymbol);
    else ans = nullptr;
    return ans;
}

HIDDEN SEXP do_dimnamesgets(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* names_)
{
    RObject* object = x_;
    if (MAYBE_SHARED(object)) object = Rf_shallow_duplicate(object);
    Rf_setAttrib(object, Symbols::DimNamesSymbol, names_);
    SETTER_CLEAR_NAMED(object);
    return object;
}

static RObject* dimnamesgets1(RObject* val1)
{
    RObject* this2;

    if (LENGTH(val1) == 0) return nullptr;
    /* if (Rf_isObject(val1)) dispatch on as.character.foo, but we don't
       have the context at this point to do so */

    if (Rf_inherits(val1, "factor"))  /* mimic as.character.factor */
	return Rf_asCharacterFactor(val1);

    if (!Rf_isString(val1)) { /* mimic as.character.default */
	PROTECT(this2 = Rf_coerceVector(val1, STRSXP));
	this2->clearAttributes();
	UNPROTECT(1);
	return this2;
    }
    return val1;
}


SEXP Rf_dimnamesgets(SEXP vec, SEXP val)
{
    SEXP dims, top, newval;
    int i, k;

    PROTECT(vec);
    PROTECT(val);

    if (!Rf_isArray(vec) && !Rf_isList(vec))
	Rf_error(_("'dimnames' applied to non-array"));
    /* This is probably overkill, but you never know; */
    /* there may be old pair-lists out there */
    /* There are, when this gets used as names<- for 1-d arrays */
    if (!Rf_isPairList(val) && !Rf_isNewList(val))
	Rf_error(_("'%s' must be a list"), "dimnames");
    dims = Rf_getAttrib(vec, Symbols::DimSymbol);
    if ((k = LENGTH(dims)) < Rf_length(val))
	Rf_error(_("length of 'dimnames' [%d] must match that of 'dims' [%d]"),
	      Rf_length(val), k);
    if (Rf_length(val) == 0) {
	removeAttrib(vec, Symbols::DimNamesSymbol);
	UNPROTECT(2);
	return vec;
    }
    /* Old list to new list */
    if (Rf_isList(val)) {
	newval = Rf_allocVector(VECSXP, k);
	for (i = 0; i < k; i++) {
	    SET_VECTOR_ELT(newval, i, CAR(val));
	    val = CDR(val);
	}
	UNPROTECT(1);
	PROTECT(val = newval);
    }
    if (Rf_length(val) > 0 && Rf_length(val) < k) {
	newval = Rf_lengthgets(val, k);
	UNPROTECT(1);
	PROTECT(val = newval);
    }
    if (MAYBE_REFERENCED(val)) {
	newval = Rf_shallow_duplicate(val);
	UNPROTECT(1);
	PROTECT(val = newval);
    }
    if (k != Rf_length(val))
	Rf_error(_("length of 'dimnames' [%d] must match that of 'dims' [%d]"),
	      Rf_length(val), k);
    for (i = 0; i < k; i++) {
	SEXP _this = VECTOR_ELT(val, i);
	if (_this != nullptr) {
	    if (!Rf_isVector(_this))
		Rf_error(_("invalid type (%s) for 'dimnames' (must be a vector)"),
		      Rf_type2char(TYPEOF(_this)));
	    if (INTEGER(dims)[i] != LENGTH(_this) && LENGTH(_this) != 0)
		Rf_error(_("length of 'dimnames' [%d] not equal to array extent"),
		      i+1);
	    SET_VECTOR_ELT(val, i, dimnamesgets1(_this));
	}
    }
    vec->setAttribute(Symbols::DimNamesSymbol, val);
    if (Rf_isList(vec) && k == 1) {
	top = VECTOR_ELT(val, 0);
	i = 0;
	for (val = vec; !Rf_isNull(val); val = CDR(val))
	    SET_TAG(val, Rf_installTrChar(STRING_ELT(top, i++)));
    }
    UNPROTECT(2);

    /* Mark as immutable so nested complex assignment can't make the
       dimnames attribute inconsistent with the length */
    MARK_NOT_MUTABLE(val);

    return vec;
}

HIDDEN SEXP do_dimnames(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_)
{
    return Rf_getAttrib(x_, Symbols::DimNamesSymbol);
}

HIDDEN SEXP do_dim(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_)
{
    return Rf_getAttrib(x_, Symbols::DimSymbol);
}

HIDDEN SEXP do_dimgets(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* value_)
{
    SEXP x = x_;
    /* Duplication might be expensive */
    if (value_ == nullptr) {
	SEXP s;
	for (s = ATTRIB(x); s != nullptr; s = CDR(s))
	    if (TAG(s) == Symbols::DimSymbol || TAG(s) == Symbols::NamesSymbol) break;
	if (s == nullptr) return x;
    }
    if (MAYBE_SHARED(x)) x = Rf_shallow_duplicate(x);
    Rf_setAttrib(x, Symbols::DimSymbol, value_);
    Rf_setAttrib(x, Symbols::NamesSymbol, nullptr);
    SETTER_CLEAR_NAMED(x);
    return x;
}

SEXP Rf_dimgets(SEXP vec, SEXP val)
{
    int i, ndim;
    R_xlen_t len, total;
    PROTECT(vec);
    PROTECT(val);
    if ((!Rf_isVector(vec) && !Rf_isList(vec)))
	Rf_error(_("invalid first argument"));

    if (!Rf_isVector(val) && !Rf_isList(val))
	Rf_error(_("invalid second argument"));
    val = Rf_coerceVector(val, INTSXP);
    UNPROTECT(1);
    PROTECT(val);

    len = Rf_xlength(vec);
    ndim = Rf_length(val);
    if (ndim == 0)
	Rf_error(_("length-0 dimension vector is invalid"));
    total = 1;
    for (i = 0; i < ndim; i++) {
	/* need this test first as R_NaInt is < 0 */
	if (INTEGER(val)[i] == R_NaInt)
	    Rf_error(_("the dims contain missing values"));
	if (INTEGER(val)[i] < 0)
	    Rf_error(_("the dims contain negative values"));
	total *= INTEGER(val)[i];
    }
    if (total != len) {
	if (total > INT_MAX || len > INT_MAX)
	    Rf_error(_("dims do not match the length of object"), total, len);
	else

	    Rf_error(_("dims [product %d] do not match the length of object [%d]"), total, len);
    }
    removeAttrib(vec, Symbols::DimNamesSymbol);
    vec->setAttribute(Symbols::DimSymbol, val);

    /* Mark as immutable so nested complex assignment can't make the
       dim attribute inconsistent with the length */
    MARK_NOT_MUTABLE(val);

    UNPROTECT(2);
    return vec;
}

HIDDEN SEXP do_attributes(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x)
{
    if (TYPEOF(x) == ENVSXP)
	R_CheckStack(); /* in case attributes might lead to a cycle */

    SEXP namesattr;
    GCStackRoot<> attrs(ATTRIB(x));
    int nvalues = Rf_length(attrs);
    if (Rf_isList(x)) {
	namesattr = Rf_getAttrib(x, Symbols::NamesSymbol);
	if (namesattr != nullptr)
	    nvalues++;
    } else
	namesattr = nullptr;
    /* FIXME */
    if (nvalues <= 0)
	return nullptr;
    /* FIXME */
    SEXP value, names;
    PROTECT(namesattr);
    PROTECT(value = Rf_allocVector(VECSXP, nvalues));
    PROTECT(names = Rf_allocVector(STRSXP, nvalues));
    nvalues = 0;
    if (namesattr != nullptr) {
	SET_VECTOR_ELT(value, nvalues, namesattr);
	SET_STRING_ELT(names, nvalues, PRINTNAME(Symbols::NamesSymbol));
	nvalues++;
    }
    while (attrs != nullptr) {
	SEXP tag = TAG(attrs);
	if (TYPEOF(tag) == SYMSXP) {
	    SET_VECTOR_ELT(value, nvalues, Rf_getAttrib(x, tag));
	    SET_STRING_ELT(names, nvalues, PRINTNAME(tag));
	}
	else { // empty tag, hence name = ""
	    MARK_NOT_MUTABLE(CAR(attrs));
	    SET_VECTOR_ELT(value, nvalues, CAR(attrs));
	    SET_STRING_ELT(names, nvalues, R_BlankString);
	}
	attrs = CDR(attrs);
	nvalues++;
    }
    Rf_setAttrib(value, Symbols::NamesSymbol, names);
    UNPROTECT(3);
    return value;
}

//  levels(.) <- newlevs :
HIDDEN SEXP do_levelsgets(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* value_)
{
    RObject* object = x_;
    RObject* levels = value_;
    if(!Rf_isNull(levels) && Rf_any_duplicated(levels, FALSE))
	Rf_errorcall(call, _("factor level [%d] is duplicated"),
		  Rf_any_duplicated(levels, FALSE));
    if (MAYBE_SHARED(object)) object = Rf_duplicate(object);
    Rf_setAttrib(object, Symbols::LevelsSymbol, levels);
    return object;
}

/* attributes(object) <- attrs */
HIDDEN SEXP do_attributesgets(/*const*/ Expression* call, const BuiltInFunction* op, RObject* object, RObject* attrs)
{
/* NOTE: The following code ensures that when an attribute list */
/* is attached to an object, that the "dim" attibute is always */
/* brought to the front of the list.  This ensures that when both */
/* "dim" and "dimnames" are set that the "dim" is attached first. */

    SEXP names = nullptr /* -Wall */;
    int i, nattrs;

    /* Do checks before duplication */
    if (!Rf_isNewList(attrs))
	Rf_error(_("attributes must be a list or NULL"));
    nattrs = Rf_length(attrs);
    if (nattrs > 0) {
	names = Rf_getAttrib(attrs, Symbols::NamesSymbol);
	if (names == nullptr)
	    Rf_error(_("attributes must be named"));
	for (i = 1; i < nattrs; i++) {
	    if (STRING_ELT(names, i) == nullptr ||
		R_CHAR(STRING_ELT(names, i))[0] == '\0') { /* all ASCII tests */
		Rf_error(_("all attributes must have names [%d does not]"), i+1);
	    }
	}
    }

    if (object == nullptr) {
	if (attrs == nullptr)
	    return nullptr;
	else
	    PROTECT(object = Rf_allocVector(VECSXP, 0));
    } else {
	/* Unlikely to have NAMED == 0 here.
	   As from R 2.7.0 we don't optimize NAMED == 1 _if_ we are
	   setting any attributes as an error later on would leave
	   'obj' changed */
	if (MAYBE_SHARED(object) || (MAYBE_REFERENCED(object) && nattrs))
	    object = Rf_shallow_duplicate(object);
	PROTECT(object);
    }


    /* Empty the existing attribute list */

    /* FIXME: the code below treats pair-based structures */
    /* in a special way.  This can probably be dropped down */
    /* the road (users should never encounter pair-based lists). */
    /* Of course, if we want backward compatibility we can't */
    /* make the change. :-( */

    if (Rf_isList(object))
	Rf_setAttrib(object, Symbols::NamesSymbol, nullptr);
    object->clearAttributes();
    /* Probably need to fix up S4 bit in other cases, but
       definitely in this one */
    if(nattrs == 0) UNSET_S4_OBJECT(object);

    /* We do two passes through the attributes; the first */
    /* finding and transferring "dim" and the second */
    /* transferring the rest.  This is to ensure that */
    /* "dim" occurs in the attribute list before "dimnames". */

    if (nattrs > 0) {
	int i0 = -1;
	for (i = 0; i < nattrs; i++) {
	    if (streql(R_CHAR(STRING_ELT(names, i)), "dim")) {
		i0 = i;
		Rf_setAttrib(object, Symbols::DimSymbol, VECTOR_ELT(attrs, i));
		break;
	    }
	}
	for (i = 0; i < nattrs; i++) {
	    if (i == i0) continue;
	    Rf_setAttrib(object, Rf_installTrChar(STRING_ELT(names, i)),
		      VECTOR_ELT(attrs, i));
	}
    }
    UNPROTECT(1);
    return object;
}

/*  This code replaces an R function defined as

    attr <- function (x, which)
    {
	if (!is.character(which))
	    stop("attribute name must be of mode character")
	if (Rf_length(which) != 1)
	    stop("exactly one attribute name must be given")
	attributes(x)[[which]]
   }

The R functions was being called very often and replacing it by
something more efficient made a noticeable difference on several
benchmarks.  There is still some inefficiency since using getAttrib
means the attributes list will be searched twice, but this seems
fairly minor.  LT */

HIDDEN SEXP do_attr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP tag = nullptr, alist, ans, x, which, exact_;
    const char *str;
    int nargs = Rf_length(args), exact = 0;
    enum match { NONE, PARTIAL, PARTIAL2, FULL } match = NONE;

    /* argument matching */
    static GCRoot<ArgMatcher> matcher
	= new ArgMatcher({ "x", "which", "exact" });
    ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::EVALUATED);
    matcher->match(arglist, { &x, &which, &exact_ });

    if (nargs < 2 || nargs > 3)
	Rf_errorcall(call, "either 2 or 3 arguments are required");

    if (!Rf_isString(which))
	Rf_errorcall(call, _("'which' must be of mode character"));
    if (Rf_length(which) != 1)
	Rf_errorcall(call, _("exactly one attribute 'which' must be given"));

    if (TYPEOF(x) == ENVSXP)
	R_CheckStack(); /* in case attributes might lead to a cycle */

    if(exact_ != R_MissingArg) {
	exact = Rf_asLogical(exact_);
	if(exact == R_NaLog) exact = 0;
    }

    if(STRING_ELT(which, 0) == R_NaString) {
	UNPROTECT(1);
	return nullptr;
    }
    str = Rf_translateChar(STRING_ELT(which, 0));
    size_t n = strlen(str);

    /* try to find a match among the attributes list */
    for (alist = ATTRIB(x); alist != nullptr; alist = CDR(alist)) {
	SEXP tmp = TAG(alist);
	const char *s = R_CHAR(PRINTNAME(tmp));
	if (! strncmp(s, str, n)) {
	    if (strlen(s) == n) {
		tag = tmp;
		match = FULL;
		break;
	    }
	    else if (match == PARTIAL || match == PARTIAL2) {
		/* this match is partial and we already have a partial match,
		   so the query is ambiguous and we will return nullptr
		   unless a full match comes up.
		*/
		match = PARTIAL2;
	    } else {
		tag = tmp;
		match = PARTIAL;
	    }
	}
    }
    if (match == PARTIAL2) {
	UNPROTECT(1);
	return nullptr;
    }

    /* Unless a full match has been found, check for a "names" attribute.
       This is stored via TAGs on pairlists, and via rownames on 1D arrays.
    */
    if (match != FULL && streqln("names", str, n)) {
	if (strlen("names") == n) {
	    /* we have a full match on "names", if there is such an
	       attribute */
	    tag = Symbols::NamesSymbol;
	    match = FULL;
	}
	else if (match == NONE && !exact) {
	    /* no match on other attributes and a possible
	       partial match on "names" */
	    tag = Symbols::NamesSymbol;
	    SEXP t = PROTECT(Rf_getAttrib(x, tag));
	    if(t != nullptr && R_warn_partial_match_attr)
		Rf_warningcall(call, _("partial match of '%s' to '%s'"), str,
			    R_CHAR(PRINTNAME(tag)));
	    UNPROTECT(2);
	    return t;
	}
	else if (match == PARTIAL && strcmp(R_CHAR(PRINTNAME(tag)), "names")) {
	    /* There is a possible partial match on "names" and on another
	       attribute. If there really is a "names" attribute, then the
	       query is ambiguous and we return nullptr.  If there is no
	       "names" attribute, then the partially matched one, which is
	       the current value of tag, can be used. */
	    if (Rf_getAttrib(x, Symbols::NamesSymbol) != nullptr) {
		UNPROTECT(1);
		return nullptr;
	    }
	}
    }

    if (match == NONE  || (exact && match != FULL)) {
	UNPROTECT(1);
	return nullptr;
    }
    if (match == PARTIAL && R_warn_partial_match_attr)
	Rf_warningcall(call, _("partial match of '%s' to '%s'"), str,
		    R_CHAR(PRINTNAME(tag)));

    ans =  Rf_getAttrib(x, tag);
    UNPROTECT(1);
    return ans;
}

static void check_slot_assign(RObject* obj, RObject* input, RObject* value, RObject* env)
{
	RObject* valueClass = PROTECT(R_data_class(value, FALSE));
	RObject* objClass   = PROTECT(R_data_class(obj, FALSE));
    static GCRoot<> checkAt = nullptr;
    // 'methods' may *not* be in search() ==> do as if calling  methods::checkAtAssignment(..)
    if(!isMethodsDispatchOn()) { // needed?
	RObject* e = PROTECT(Rf_lang1(Rf_install("initMethodDispatch")));
	Rf_eval(e, R_MethodsNamespace); // only works with methods loaded
	UNPROTECT(1);
    }
    if(checkAt == nullptr)
	checkAt = Rf_findFun(Rf_install("checkAtAssignment"), R_MethodsNamespace);
    RObject* e = PROTECT(Rf_lang4(checkAt, objClass, input, valueClass));
    Rf_eval(e, env);
    UNPROTECT(3);
}


HIDDEN SEXP do_slotgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /*  attr@nlist  <-  value  */
    ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::RAW);
    SEXP input;

    SEXP nlist = arglist.get(1);
    if (Rf_isSymbol(nlist))
	input = Rf_ScalarString(PRINTNAME(nlist));
    else if(Rf_isString(nlist)) {
        input = Rf_length(nlist) == 1
            ? nlist : Rf_ScalarString(STRING_ELT(nlist, 0));
    }
    else {
	Rf_error(_("invalid type '%s' for slot name"),
	      Rf_type2char(TYPEOF(nlist)));
	return nullptr; /*-Wall*/
    }
    PROTECT(input);

    /* replace the second argument with a string */
    arglist.set(1, input);
    UNPROTECT(1); // 'input' is now protected

    auto disptached = Rf_DispatchOrEval(SEXP_downcast<Expression*>(call),
                                        SEXP_downcast<BuiltInFunction*>(op),
                                        &arglist,
                                        SEXP_downcast<Environment*>(env),
                                        MissingArgHandling::Keep);
    if (disptached.first)
        return disptached.second;

    RObject* obj = arglist.get(0);
    RObject* value = arglist.get(2);
    check_slot_assign(obj, input, value, env);
    value = R_do_slot_assign(obj, input, value);
    UNPROTECT(2);
    return value;
}

HIDDEN SEXP do_attrgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /*  attr(x, which = "<name>")  <-  value  */
    SEXP obj = CAR(args);
    if (MAYBE_SHARED(obj))
	PROTECT(obj = Rf_shallow_duplicate(obj));
    else
	PROTECT(obj);

    /* argument matching */
    static GCRoot<ArgMatcher> matcher
	= new ArgMatcher({ "x", "which", "value" });
    ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::EVALUATED);
    SEXP ignored, name, value;
    matcher->match(arglist, { &ignored, &name, &value });

    if (!Rf_isValidString(name) || STRING_ELT(name, 0) == R_NaString)
	Rf_error(_("'name' must be non-null character string"));
    /* TODO?  if (Rf_isFactor(obj) && streql(Rf_asChar(name), "levels"))
     * ---         if(Rf_any_duplicated(CADDR(args)))
     *                  Rf_error(.....)
     */
    Rf_setAttrib(obj, name, value);
    UNPROTECT(1);
    SETTER_CLEAR_NAMED(obj);
    return obj;
}


/* These provide useful shortcuts which give access to */
/* the dimnames for matrices and arrays in a standard form. */

/* NB: this may return R_alloc-ed rn and dn */
void Rf_GetMatrixDimnames(SEXP x, SEXP *rl, SEXP *cl,
		       const char **rn, const char **cn)
{
    SEXP dimnames = Rf_getAttrib(x, Symbols::DimNamesSymbol);
    SEXP nn;

    if (Rf_isNull(dimnames)) {
	*rl = nullptr;
	*cl = nullptr;
	*rn = nullptr;
	*cn = nullptr;
    }
    else {
	*rl = VECTOR_ELT(dimnames, 0);
	*cl = VECTOR_ELT(dimnames, 1);
	nn = Rf_getAttrib(dimnames, Symbols::NamesSymbol);
	if (Rf_isNull(nn)) {
	    *rn = nullptr;
	    *cn = nullptr;
	}
	else {
	    *rn = Rf_translateChar(STRING_ELT(nn, 0));
	    *cn = Rf_translateChar(STRING_ELT(nn, 1));
	}
    }
}


SEXP Rf_GetArrayDimnames(SEXP x)
{
    return Rf_getAttrib(x, Symbols::DimNamesSymbol);
}


/* the code to manage slots in formal classes. These are attributes,
   but without partial matching and enforcing legal slot names (it's
   an error to get a slot that doesn't exist. */


static GCRoot<> pseudo_NULL = nullptr;

static GCRoot<> s_dot_Data;
static GCRoot<> s_getDataPart;
static GCRoot<> s_setDataPart;

static void init_slot_handling(void) {
    s_dot_Data = Rf_install(".Data");
    s_dot_S3Class = Rf_install(".S3Class");
    s_getDataPart = Rf_install("getDataPart");
    s_setDataPart = Rf_install("setDataPart");
    /* create and preserve an object that is NOT nullptr, and is used
       to represent slots that are NULL (which an attribute can not
       be).  The point is not just to store NULL as a slot, but also to
       provide a check on invalid slot names (see get_slot below).

       The object has to be a symbol if we're going to check identity by
       just looking at referential equality. */
    pseudo_NULL = Rf_install("\001NULL\001");
}

static RObject* data_part(RObject* obj) {
    RObject* e;
    RObject* val;
    if(!s_getDataPart)
	init_slot_handling();
    PROTECT(e = Rf_allocVector(LANGSXP, 2));
    SETCAR(e, s_getDataPart);
    val = CDR(e);
    SETCAR(val, obj);
    val = Rf_eval(e, R_MethodsNamespace);
    UNSET_S4_OBJECT(val); /* data part must be base vector */
    UNPROTECT(1);
    return(val);
}

static RObject* set_data_part(RObject* obj,  RObject* rhs) {
    RObject* e;
    RObject* val;
    if(!s_setDataPart)
	init_slot_handling();
    PROTECT(e = Rf_allocVector(LANGSXP, 3));
    SETCAR(e, s_setDataPart);
    val = CDR(e);
    SETCAR(val, obj);
    val = CDR(val);
    SETCAR(val, rhs);
    val = Rf_eval(e, R_MethodsNamespace);
    SET_S4_OBJECT(val);
    UNPROTECT(1);
    return(val);
}

SEXP Rf_S3Class(SEXP obj)
{
    if(!s_dot_S3Class) init_slot_handling();
    return Rf_getAttrib(obj, s_dot_S3Class);
}

/* Slots are stored as attributes to
   provide some back-compatibility
*/

/**
 * R_has_slot() : a C-level test if a obj@<name> is available;
 *                as R_do_slot() gives an error when there's no such slot.
 */
int R_has_slot(SEXP obj, SEXP name) {

#define R_SLOT_INIT							\
    if(!(Rf_isSymbol(name) || (Rf_isString(name) && LENGTH(name) == 1)))	\
	Rf_error(_("invalid type or length for slot name"));		\
    if(!s_dot_Data)							\
	init_slot_handling();						\
    if(Rf_isString(name)) name = Rf_installTrChar(STRING_ELT(name, 0))

    R_SLOT_INIT;
    if(name == s_dot_Data && TYPEOF(obj) != S4SXP)
	return(1);
    /* else */
    return(Rf_getAttrib(obj, name) != nullptr);
}

/* the @ operator, and its assignment form.  Processed much like $
   (see do_subset3) but without S3-style methods.
*/
/* currently, R_get_slot() ["methods"] is a trivial wrapper for this: */
SEXP R_do_slot(SEXP obj, SEXP name) {
    R_SLOT_INIT;
    if(name == s_dot_Data)
	return data_part(obj);
    else {
	SEXP value = Rf_getAttrib(obj, name);
	if(value == nullptr) {
	    SEXP input = name, classString;
	    if(name == s_dot_S3Class) /* defaults to class(obj) */
		return R_data_class(obj, FALSE);
	    else if(name == Symbols::NamesSymbol &&
		    TYPEOF(obj) == VECSXP) /* needed for namedList class */
		return value;
	    if(Rf_isSymbol(name) ) {
		input = PROTECT(Rf_ScalarString(PRINTNAME(name)));
		classString = Rf_getAttrib(obj, Symbols::ClassSymbol);
		if(Rf_isNull(classString)) {
		    UNPROTECT(1);
		    Rf_error(_("cannot get a slot (\"%s\") from an object of type \"%s\""),
			  Rf_translateChar(Rf_asChar(input)),
			  R_CHAR(Rf_type2str(TYPEOF(obj))));
		}
		UNPROTECT(1);
	    }
	    else classString = nullptr; /* make sure it is initialized */
	    /* not there.  But since even NULL really does get stored, this
	       implies that there is no slot of this name.  Or somebody
	       screwed up by using attr(..) <- NULL */

	    Rf_error(_("no slot of name \"%s\" for this object of class \"%s\""),
		  Rf_translateChar(Rf_asChar(input)),
		  Rf_translateChar(Rf_asChar(classString)));
	}
	else if(value == pseudo_NULL)
	    value = nullptr;
	return value;
    }
}
#undef R_SLOT_INIT

/* currently, R_set_slot() ["methods"] is a trivial wrapper for this: */
SEXP R_do_slot_assign(SEXP obj, SEXP name, SEXP value) {
#ifndef _R_ver_le_2_11_x_
    if (Rf_isNull(obj))/* cannot use !IS_S4_OBJECT(obj), because
		     *  slot(obj, name, check=FALSE) <- value  must work on
		     * "pre-objects", currently only in makePrototypeFromClassDef() */
	Rf_error(_("attempt to set slot on NULL object"));
#endif
    PROTECT(obj); PROTECT(value);
                                /* Ensure that name is a symbol */
    if(Rf_isString(name) && LENGTH(name) == 1)
	name = Rf_installTrChar(STRING_ELT(name, 0));
    else if(TYPEOF(name) == CHARSXP)
	name = Rf_installTrChar(name);
    if(!Rf_isSymbol(name) )
	Rf_error(_("invalid type or length for slot name"));

    if(!s_dot_Data)		/* initialize */
	init_slot_handling();

    if(name == s_dot_Data) {	/* special handling */
	obj = set_data_part(obj, value);
    } else {
	if(Rf_isNull(value))		/* Slots, but not attributes, can be NULL.*/
	    value = pseudo_NULL;	/* Store a special symbol instead. */

#ifdef _R_ver_le_2_11_x_
	Rf_setAttrib(obj, name, value);
#else
	/* simplified version of Rf_setAttrib(obj, name, value);
	   here we do *not* treat "names", "dimnames", "dim", .. specially : */
	PROTECT(name);
	if (MAYBE_REFERENCED(value)) value = R_FixupRHS(obj, value);
	UNPROTECT(1);
	obj->setAttribute(SEXP_downcast<Symbol*>(name), value);
#endif
    }
    UNPROTECT(2);
    return obj;
}

HIDDEN SEXP do_AT(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  nlist, object, ans, klass;

    checkArity(op, args);
    if(!isMethodsDispatchOn())
	Rf_error(_("formal classes cannot be used without the 'methods' package"));
    nlist = CADR(args);
    /* Do some checks here -- repeated in R_do_slot, but on repeat the
     * test expression should kick out on the first element. */
    if(!(Rf_isSymbol(nlist) || (Rf_isString(nlist) && LENGTH(nlist) == 1)))
	Rf_error(_("invalid type or length for slot name"));
    if(Rf_isString(nlist)) nlist = Rf_installTrChar(STRING_ELT(nlist, 0));
    PROTECT(object = Rf_eval(CAR(args), env));
    if(!s_dot_Data) init_slot_handling();
    if(nlist != s_dot_Data && !IS_S4_OBJECT(object)) {
	klass = Rf_getAttrib(object, Symbols::ClassSymbol);
	if(Rf_length(klass) == 0)
	    Rf_error(_("trying to get slot \"%s\" from an object of a basic class (\"%s\") with no slots"),
		  R_CHAR(PRINTNAME(nlist)),
		  R_CHAR(STRING_ELT(R_data_class(object, FALSE), 0)));
	else
	    Rf_error(_("trying to get slot \"%s\" from an object (class \"%s\") that is not an S4 object "),
		  R_CHAR(PRINTNAME(nlist)),
		  Rf_translateChar(STRING_ELT(klass, 0)));
    }

    ans = R_do_slot(object, nlist);
    UNPROTECT(1);
    return ans;
}

/* Return a suitable S3 object (OK, the name of the routine comes from
   an earlier version and isn't quite accurate.) If there is a .S3Class
   slot convert to that S3 class.
   Otherwise, unless type == S4SXP, look for a .Data or .xData slot.  The
   value of type controls what's wanted.  If it is S4SXP, then ONLY
   .S3class is used.  If it is ANYSXP, don't check except that automatic
   conversion from the current type only applies for classes that extend
   one of the basic types (i.e., not S4SXP).  For all other types, the
   recovered data must match the type.
   Because S3 objects can't have type S4SXP, .S3Class slot is not searched
   for in that type object, unless ONLY that class is wanted.
   (Obviously, this is another routine that has accumulated barnacles and
   should at some time be broken into separate parts.)
*/
HIDDEN SEXP
R_getS4DataSlot(SEXP obj, SEXPTYPE type)
{
  static Symbol* s_xData = Symbol::obtain(".xData");
  static Symbol* s_dotData = Symbol::obtain(".Data");

  SEXP value = nullptr;
  PROTECT_INDEX opi;

  PROTECT_WITH_INDEX(obj, &opi);

  if(TYPEOF(obj) != S4SXP || type == S4SXP) {
    SEXP s3class = Rf_S3Class(obj);
    if(s3class == nullptr && type == S4SXP) {
      UNPROTECT(1); /* obj */
      return nullptr;
    }
    PROTECT(s3class);
    if(MAYBE_REFERENCED(obj))
      REPROTECT(obj = Rf_shallow_duplicate(obj), opi);
    if(s3class != nullptr) {/* replace class with S3 class */
      Rf_setAttrib(obj, Symbols::ClassSymbol, s3class);
      Rf_setAttrib(obj, s_dot_S3Class, nullptr); /* not in the S3 class */
    }
    else { /* to avoid inf. recursion, must unset class attribute */
      Rf_setAttrib(obj, Symbols::ClassSymbol, nullptr);
    }
    UNPROTECT(1); /* s3class */
    UNSET_S4_OBJECT(obj);
    if(type == S4SXP) {
      UNPROTECT(1); /* obj */
      return obj;
    }
    value = obj;
  }
  else
      value = Rf_getAttrib(obj, s_dotData);
  if(value == nullptr)
      value = Rf_getAttrib(obj, s_xData);

  UNPROTECT(1); /* obj */
/* the mechanism for extending abnormal types.  In the future, would b
   good to consolidate under the ".Data" slot, but this has
   been used to mean S4 objects with non-S4 type, so for now
   a secondary slot name, ".xData" is used to avoid confusion
*/
  if(value != nullptr &&
     (type == ANYSXP || type == TYPEOF(value)))
     return value;
  else
     return nullptr;
}
