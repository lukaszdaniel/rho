/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2016--2017   The R Core Team
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
#include <R_ext/Altrep.h>
#include <float.h> /* for DBL_DIG */
#include <Print.h> /* for R_print */
#include <R_ext/Itermacros.h>
#include <rho/PairList.hpp>
#include <rho/WeakRef.hpp>

using namespace rho;

/**
 **  ALTREP Class Registry for Serialization
 **/

/* Use ATTRIB field to hold class info. OK since not visible outside. */
inline static SEXP ALTREP_CLASS_SERIALIZED_CLASS(SEXP x) { return ATTRIB(x); }
inline static void SET_ALTREP_CLASS_SERIALIZED_CLASS(
    SEXP x, SEXP csym, SEXP psym, SEXP stype)
{
    SET_ATTRIB(x, Rf_list3(csym, psym, stype));
}
inline static SEXP ALTREP_SERIALIZED_CLASS_CLSSYM(SEXP x) { return CAR(x); }
inline static SEXP ALTREP_SERIALIZED_CLASS_PKGSYM(SEXP x) { return CADR(x); }
inline static SEXPTYPE ALTREP_SERIALIZED_CLASS_TYPE(SEXP x)
{
    return SEXPTYPE(INTEGER0(CADDR(x))[0]);
}

inline static SEXPTYPE ALTREP_CLASS_BASE_TYPE(SEXP x)
{
    return ALTREP_SERIALIZED_CLASS_TYPE(ALTREP_CLASS_SERIALIZED_CLASS(x));
}

static SEXP Registry = nullptr;

static SEXP LookupClassEntry(SEXP csym, SEXP psym)
{
    for (SEXP chain = CDR(Registry); chain != nullptr; chain = CDR(chain))
	if (TAG(CAR(chain)) == csym && CADR(CAR(chain)) == psym)
	    return CAR(chain);
    return nullptr;
}

static void
RegisterClass(SEXP class_, SEXPTYPE type, const char *cname, const char *pname,
	      DllInfo *dll)
{
    PROTECT(class_);
    if (Registry == nullptr) {
	Registry = PairList::cons(nullptr, nullptr);
	R_PreserveObject(Registry);
    }

    SEXP csym = rho::Symbol::obtain(cname);
    SEXP psym = rho::Symbol::obtain(pname);
    SEXP stype = PROTECT(Rf_ScalarInteger(type));
    SEXP iptr = R_MakeExternalPtr(dll, nullptr, nullptr);
    SEXP entry = LookupClassEntry(csym, psym);
    if (entry == nullptr) {
	entry = Rf_list4(class_, psym, stype, iptr);
	SET_TAG(entry, csym);
	SETCDR(Registry, Rf_cons(entry, CDR(Registry)));
    }
    else {
	SETCAR(entry, class_);
	SETCAR(CDR(CDR(entry)), stype);
	SETCAR(CDR(CDR(CDR(entry))), iptr);
    }
    SET_ALTREP_CLASS_SERIALIZED_CLASS(class_, csym, psym, stype);
    UNPROTECT(2); /* class_, stype */
}

static SEXP LookupClass(SEXP csym, SEXP psym)
{
    SEXP entry = LookupClassEntry(csym, psym);
    return entry != nullptr ? CAR(entry) : nullptr;
}

static void reinit_altrep_class(SEXP sclass);
HIDDEN void R_reinit_altrep_classes(DllInfo *dll)
{
    for (SEXP chain = CDR(Registry); chain != nullptr; chain = CDR(chain)) {
	SEXP entry = CAR(chain);
	SEXP iptr = CAR(CDR(CDR(CDR(entry))));
	if (R_ExternalPtrAddr(iptr) == dll)
	    reinit_altrep_class(CAR(entry));
    }
}


/**
 **  ALTREP Method Tables and Class Objects
 **/

static void SET_ALTREP_CLASS(SEXP x, SEXP class_)
{
    SETALTREP(x, 1);
    SET_TAG(x, class_);
}

#define CLASS_METHODS_TABLE(class_) STDVEC_DATAPTR(class_)
#define GENERIC_METHODS_TABLE(x, class_) \
    ((class_##_methods_t *) CLASS_METHODS_TABLE(ALTREP_CLASS(x)))

#define ALTREP_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altrep)
#define ALTVEC_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altvec)
#define ALTINTEGER_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altinteger)
#define ALTREAL_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altreal)
#define ALTSTRING_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altstring)

#define ALTREP_METHODS						\
    R_altrep_UnserializeEX_method_t UnserializeEX;		\
    R_altrep_Unserialize_method_t Unserialize;			\
    R_altrep_Serialized_state_method_t Serialized_state;	\
    R_altrep_DuplicateEX_method_t DuplicateEX;			\
    R_altrep_Duplicate_method_t Duplicate;			\
    R_altrep_Coerce_method_t Coerce;				\
    R_altrep_Inspect_method_t Inspect;				\
    R_altrep_Length_method_t Length

#define ALTVEC_METHODS					\
    ALTREP_METHODS;					\
    R_altvec_Dataptr_method_t Dataptr;			\
    R_altvec_Dataptr_or_null_method_t Dataptr_or_null;	\
    R_altvec_Extract_subset_method_t Extract_subset

#define ALTINTEGER_METHODS				\
    ALTVEC_METHODS;					\
    R_altinteger_Elt_method_t Elt;			\
    R_altinteger_Get_region_method_t Get_region;	\
    R_altinteger_Is_sorted_method_t Is_sorted;		\
    R_altinteger_No_NA_method_t No_NA;			\
    R_altinteger_Sum_method_t Sum ;			\
    R_altinteger_Min_method_t Min;			\
    R_altinteger_Max_method_t Max

#define ALTREAL_METHODS				\
    ALTVEC_METHODS;				\
    R_altreal_Elt_method_t Elt;			\
    R_altreal_Get_region_method_t Get_region;	\
    R_altreal_Is_sorted_method_t Is_sorted;	\
    R_altreal_No_NA_method_t No_NA;		\
    R_altreal_Sum_method_t Sum;			\
    R_altreal_Min_method_t Min;			\
    R_altreal_Max_method_t Max

#define ALTSTRING_METHODS			\
    ALTVEC_METHODS;				\
    R_altstring_Elt_method_t Elt;		\
    R_altstring_Set_elt_method_t Set_elt;	\
    R_altstring_Is_sorted_method_t Is_sorted;	\
    R_altstring_No_NA_method_t No_NA

typedef struct { ALTREP_METHODS; } altrep_methods_t;
typedef struct { ALTVEC_METHODS; } altvec_methods_t;
typedef struct { ALTINTEGER_METHODS; } altinteger_methods_t;
typedef struct { ALTREAL_METHODS; } altreal_methods_t;
typedef struct { ALTSTRING_METHODS; } altstring_methods_t;

/* Macro to extract first element from ... macro argument.
   From Richard Hansen's answer in
   http://stackoverflow.com/questions/5588855/standard-alternative-to-gccs-va-args-trick 
*/
#define DISPATCH_TARGET(...) DISPATCH_TARGET_HELPER(__VA_ARGS__, dummy)
#define DISPATCH_TARGET_HELPER(x, ...) x

#define DO_DISPATCH(type, fun, ...)					\
    type##_METHODS_TABLE(DISPATCH_TARGET(__VA_ARGS__))->fun(__VA_ARGS__)

#define ALTREP_DISPATCH(fun, ...) DO_DISPATCH(ALTREP, fun, __VA_ARGS__)
#define ALTVEC_DISPATCH(fun, ...) DO_DISPATCH(ALTVEC, fun, __VA_ARGS__)
#define ALTINTEGER_DISPATCH(fun, ...) DO_DISPATCH(ALTINTEGER, fun, __VA_ARGS__)
#define ALTREAL_DISPATCH(fun, ...) DO_DISPATCH(ALTREAL, fun, __VA_ARGS__)
#define ALTSTRING_DISPATCH(fun, ...) DO_DISPATCH(ALTSTRING, fun, __VA_ARGS__)


/*
 * Generic ALTREP support
 */

HIDDEN SEXP ALTREP_COERCE(SEXP x, int type)
{
    return ALTREP_DISPATCH(Coerce, x, type);
}

static SEXP ALTREP_DUPLICATE(SEXP x, Rboolean deep)
{
    return ALTREP_DISPATCH(Duplicate, x, deep);
}

HIDDEN SEXP ALTREP_DUPLICATE_EX(SEXP x, Rboolean deep)
{
    return ALTREP_DISPATCH(DuplicateEX, x, deep);
}

HIDDEN Rboolean
ALTREP_INSPECT(SEXP x, int pre, int deep, int pvec,
	       void (*inspect_subtree)(SEXP, int, int, int))
{
    return ALTREP_DISPATCH(Inspect, x, pre, deep, pvec, inspect_subtree);
}


HIDDEN SEXP
ALTREP_SERIALIZED_STATE(SEXP x)
{
    return ALTREP_DISPATCH(Serialized_state, x);
}

HIDDEN SEXP
ALTREP_SERIALIZED_CLASS(SEXP x)
{
    SEXP val = ALTREP_CLASS_SERIALIZED_CLASS(ALTREP_CLASS(x));
    return val ? val : nullptr;
}

static SEXP find_namespace(void *data) { return R_FindNamespace((SEXP) data); }
static SEXP handle_namespace_error(SEXP cond, void *data) { return nullptr; }

static SEXP ALTREP_UNSERIALIZE_CLASS(SEXP info)
{
    if (TYPEOF(info) == LISTSXP) {
	SEXP csym = ALTREP_SERIALIZED_CLASS_CLSSYM(info);
	SEXP psym = ALTREP_SERIALIZED_CLASS_PKGSYM(info);
	SEXP class_ = LookupClass(csym, psym);
	if (class_ == nullptr) {
	    SEXP pname = Rf_ScalarString(PRINTNAME(psym));
	    R_tryCatchError(find_namespace, pname,
			    handle_namespace_error, nullptr);
	    class_ = LookupClass(csym, psym);
	}
	return class_;
    }
    return nullptr;
}

HIDDEN SEXP
ALTREP_UNSERIALIZE_EX(SEXP info, SEXP state, SEXP attr, int objf, int levs)
{
    SEXP csym = ALTREP_SERIALIZED_CLASS_CLSSYM(info);
    SEXP psym = ALTREP_SERIALIZED_CLASS_PKGSYM(info);
    SEXPTYPE type = ALTREP_SERIALIZED_CLASS_TYPE(info);

    /* look up the class in the registry and handle failure */
    SEXP class_ = ALTREP_UNSERIALIZE_CLASS(info);
    if (class_ == nullptr) {
	switch(type) {
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	case RAWSXP:
	case VECSXP:
	case EXPRSXP:
	    Rf_warning("cannot unserialize ALTVEC object of class '%s' from package '%s'; returning length zero vector",
		    R_CHAR(PRINTNAME(csym)), R_CHAR(PRINTNAME(psym)));
	    return Rf_allocVector(type, 0);
	default:
	    Rf_error("cannot unserialize this ALTREP object");
	}
    }

    /* check the registered and unserialized types match */
    SEXPTYPE rtype = ALTREP_CLASS_BASE_TYPE(class_);
    if (type != rtype)
	Rf_warning("serialized class '%s' from package '%s' has type %s; registered class has type %s",
		R_CHAR(PRINTNAME(csym)), R_CHAR(PRINTNAME(psym)),
		Rf_type2char(type), Rf_type2char(rtype));

    /* dispatch to a class method */
    altrep_methods_t *m = static_cast<altrep_methods_t *>(CLASS_METHODS_TABLE(class_));
    SEXP val = m->UnserializeEX(class_, state, attr, objf, levs);
    return val;
}

R_xlen_t /*HIDDEN*/ ALTREP_LENGTH(SEXP x)
{
    return ALTREP_DISPATCH(Length, x);
}

R_xlen_t /*HIDDEN*/ ALTREP_TRUELENGTH(SEXP x) { return 0; }


/*
 * Generic ALTVEC support
 */

R_INLINE static void* ALTVEC_DATAPTR_EX(SEXP x, Rboolean writeable)
{
    /**** move GC disabling into methods? */
#if RHO_FALSE
    if (R_in_gc)
	Rf_error("cannot get ALTVEC DATAPTR during GC");
    int enabled = R_GCEnabled;
    R_GCEnabled = FALSE;
#endif
    void *val = ALTVEC_DISPATCH(Dataptr, x, writeable);

    // R_GCEnabled = enabled;
    return val;
}

void /*HIDDEN*/ *ALTVEC_DATAPTR(SEXP x)
{
    return ALTVEC_DATAPTR_EX(x, TRUE);
}

const void /*HIDDEN*/ *ALTVEC_DATAPTR_RO(SEXP x)
{
    return ALTVEC_DATAPTR_EX(x, FALSE);
}

const void /*HIDDEN*/ *ALTVEC_DATAPTR_OR_NULL(SEXP x)
{
    return ALTVEC_DISPATCH(Dataptr_or_null, x);
}

HIDDEN SEXP ALTVEC_EXTRACT_SUBSET(SEXP x, SEXP indx, SEXP call)
{
    return ALTVEC_DISPATCH(Extract_subset, x, indx, call);
}


/*
 * Typed ALTVEC support
 */

#define CHECK_NOT_EXPANDED(x)					\
    if (DATAPTR_OR_NULL(x) != nullptr)				\
	Rf_error("method should only handle unexpanded vectors")

HIDDEN int ALTINTEGER_ELT(SEXP x, R_xlen_t i)
{
    return ALTINTEGER_DISPATCH(Elt, x, i);
}

R_xlen_t INTEGER_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf)
{
    const int *x = INTEGER_OR_NULL(sx);
    if (x != nullptr) {
	R_xlen_t size = XLENGTH(sx);
	R_xlen_t ncopy = size - i > n ? n : size - i;
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = x[k + i];
	//memcpy(buf, x + i, ncopy * sizeof(int));
	return ncopy;
    }
    else
	return ALTINTEGER_DISPATCH(Get_region, sx, i, n, buf);
}

int INTEGER_IS_SORTED(SEXP x)
{
    return ALTREP(x) ? ALTINTEGER_DISPATCH(Is_sorted, x) : UNKNOWN_SORTEDNESS;
}

int INTEGER_NO_NA(SEXP x)
{
    return ALTREP(x) ? ALTINTEGER_DISPATCH(No_NA, x) : 0;
}

HIDDEN double ALTREAL_ELT(SEXP x, R_xlen_t i)
{
    return ALTREAL_DISPATCH(Elt, x, i);
}

R_xlen_t REAL_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, double *buf)
{
    const double *x = REAL_OR_NULL(sx);
    if (x != nullptr) {
	R_xlen_t size = XLENGTH(sx);
	R_xlen_t ncopy = size - i > n ? n : size - i;
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = x[k + i];
	//memcpy(buf, x + i, ncopy * sizeof(double));
	return ncopy;
    }
    else
	return ALTREAL_DISPATCH(Get_region, sx, i, n, buf);
}

int REAL_IS_SORTED(SEXP x)
{
    return ALTREP(x) ? ALTREAL_DISPATCH(Is_sorted, x) : UNKNOWN_SORTEDNESS;
}

int REAL_NO_NA(SEXP x)
{
    return ALTREP(x) ? ALTREAL_DISPATCH(No_NA, x) : 0;
}

SEXP /*HIDDEN*/ ALTSTRING_ELT(SEXP x, R_xlen_t i)
{
    SEXP val = nullptr;

    /**** move GC disabling into method? */
#if RHO_FALSE
    if (R_in_gc)
	Rf_error("cannot get ALTSTRING_ELT during GC");
    int enabled = R_GCEnabled;
    R_GCEnabled = FALSE;
#endif
    val = ALTSTRING_DISPATCH(Elt, x, i);

    // R_GCEnabled = enabled;
    return val;
}

HIDDEN void ALTSTRING_SET_ELT(SEXP x, R_xlen_t i, SEXP v)
{
    /**** move GC disabling into method? */
#if RHO_FALSE
    if (R_in_gc)
	Rf_error("cannot get ALTSTRING_ELT during GC");
    int enabled = R_GCEnabled;
    R_GCEnabled = FALSE;
#endif
    ALTSTRING_DISPATCH(Set_elt, x, i, v);

    // R_GCEnabled = enabled;
}

int STRING_IS_SORTED(SEXP x)
{
    return ALTREP(x) ? ALTSTRING_DISPATCH(Is_sorted, x) : UNKNOWN_SORTEDNESS;
}

int STRING_NO_NA(SEXP x)
{
    return ALTREP(x) ? ALTSTRING_DISPATCH(No_NA, x) : 0;
}

SEXP ALTINTEGER_SUM(SEXP x, Rboolean narm)
{
    return ALTINTEGER_DISPATCH(Sum, x, narm);
}

SEXP ALTINTEGER_MIN(SEXP x, Rboolean narm)
{
    return ALTINTEGER_DISPATCH(Min, x, narm);
}

SEXP ALTINTEGER_MAX(SEXP x, Rboolean narm)
{
    return ALTINTEGER_DISPATCH(Max, x, narm);

}

SEXP ALTREAL_SUM(SEXP x, Rboolean narm)
{
    return ALTREAL_DISPATCH(Sum, x, narm);
}

SEXP ALTREAL_MIN(SEXP x, Rboolean narm)
{
    return ALTREAL_DISPATCH(Min, x, narm);
}

SEXP ALTREAL_MAX(SEXP x, Rboolean narm)
{
    return ALTREAL_DISPATCH(Max, x, narm);

}


/*
 * Not yet implemented
 */

HIDDEN int ALTLOGICAL_ELT(SEXP x, R_xlen_t i)
{
    return LOGICAL(x)[i]; /* dispatch here */
}

HIDDEN Rcomplex ALTCOMPLEX_ELT(SEXP x, R_xlen_t i)
{
    return COMPLEX(x)[i]; /* dispatch here */
}

HIDDEN Rbyte ALTRAW_ELT(SEXP x, R_xlen_t i)
{
    return RAW(x)[i]; /* dispatch here */
}

void ALTINTEGER_SET_ELT(SEXP x, R_xlen_t i, int v)
{
    INTEGER(x)[i] = v; /* dispatch here */
}

void ALTLOGICAL_SET_ELT(SEXP x, R_xlen_t i, int v)
{
    LOGICAL(x)[i] = v; /* dispatch here */
}

void ALTREAL_SET_ELT(SEXP x, R_xlen_t i, double v)
{
    REAL(x)[i] = v; /* dispatch here */
}

void ALTCOMPLEX_SET_ELT(SEXP x, R_xlen_t i, Rcomplex v)
{
    COMPLEX(x)[i] = v; /* dispatch here */
}

void ALTRAW_SET_ELT(SEXP x, R_xlen_t i, int v)
{
    RAW(x)[i] = (Rbyte) v; /* dispatch here */
}


/**
 ** ALTREP Default Methods
 **/

static SEXP altrep_UnserializeEX_default(SEXP class_, SEXP state, SEXP attr,
					 int objf, int levs)
{
    altrep_methods_t *m = static_cast<altrep_methods_t *>(CLASS_METHODS_TABLE(class_));
    SEXP val = m->Unserialize(class_, state);
    SET_ATTRIB(val, attr);
    SET_OBJECT(val, objf);
    SETLEVELS(val, levs);
    return val;
}

static SEXP altrep_Serialized_state_default(SEXP x) { return nullptr; }

static SEXP altrep_Unserialize_default(SEXP class_, SEXP state)
{
    Rf_error("cannot unserialize this ALTREP object yet");
}

static SEXP altrep_Coerce_default(SEXP x, int type) { return nullptr; }

static SEXP altrep_Duplicate_default(SEXP x, Rboolean deep)
{
    return nullptr;
}

static SEXP altrep_DuplicateEX_default(SEXP x, Rboolean deep)
{
    SEXP ans = ALTREP_DUPLICATE(x, deep);

    if (ans != nullptr &&
	ans != x) { /* leave attributes alone if returning original */
	/* handle attributes generically */
	SEXP attr = ATTRIB(x);
	if (attr != nullptr) {
	    PROTECT(ans);
	    SET_ATTRIB(ans, deep ? Rf_duplicate(attr) : Rf_shallow_duplicate(attr));
	    SET_OBJECT(ans, OBJECT(x));
	    IS_S4_OBJECT(x) ? SET_S4_OBJECT(ans) : UNSET_S4_OBJECT(ans);
	    UNPROTECT(1);
	}
    }
    return ans;
}

static
Rboolean altrep_Inspect_default(SEXP x, int pre, int deep, int pvec,
				void (*inspect_subtree)(SEXP, int, int, int))
{
    return FALSE;
}

static R_xlen_t altrep_Length_default(SEXP x)
{
    Rf_error("no Length method defined");
}

static void *altvec_Dataptr_default(SEXP x, Rboolean writeable)
{
    /**** use class info for better error message? */
    Rf_error("cannot access data pointer for this ALTVEC object");
}

static const void *altvec_Dataptr_or_null_default(SEXP x)
{
    return nullptr;
}

static SEXP altvec_Extract_subset_default(SEXP x, SEXP indx, SEXP call)
{
    return nullptr;
}

static int altinteger_Elt_default(SEXP x, R_xlen_t i) { return INTEGER(x)[i]; }

static R_xlen_t
altinteger_Get_region_default(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf)
{
    R_xlen_t size = XLENGTH(sx);
    R_xlen_t ncopy = size - i > n ? n : size - i;
    for (R_xlen_t k = 0; k < ncopy; k++)
	buf[k] = INTEGER_ELT(sx, k + i);
    return ncopy;
}

static int altinteger_Is_sorted_default(SEXP x) { return UNKNOWN_SORTEDNESS; }
static int altinteger_No_NA_default(SEXP x) { return 0; }

static SEXP altinteger_Sum_default(SEXP x, Rboolean narm) { return nullptr; }
static SEXP altinteger_Min_default(SEXP x, Rboolean narm) { return nullptr; }
static SEXP altinteger_Max_default(SEXP x, Rboolean narm) { return nullptr; }

static double altreal_Elt_default(SEXP x, R_xlen_t i) { return REAL(x)[i]; }

static R_xlen_t
altreal_Get_region_default(SEXP sx, R_xlen_t i, R_xlen_t n, double *buf)
{
    R_xlen_t size = XLENGTH(sx);
    R_xlen_t ncopy = size - i > n ? n : size - i;
    for (R_xlen_t k = 0; k < ncopy; k++)
	buf[k] = REAL_ELT(sx, k + i);
    return ncopy;
}

static int altreal_Is_sorted_default(SEXP x) { return UNKNOWN_SORTEDNESS; }
static int altreal_No_NA_default(SEXP x) { return 0; }

static SEXP altreal_Sum_default(SEXP x, Rboolean narm) { return nullptr; }
static SEXP altreal_Min_default(SEXP x, Rboolean narm) { return nullptr; }
static SEXP altreal_Max_default(SEXP x, Rboolean narm) { return nullptr; }

static SEXP altstring_Elt_default(SEXP x, R_xlen_t i)
{
    Rf_error("ALTSTRING classes must provide an Elt method");
}

static void altstring_Set_elt_default(SEXP x, R_xlen_t i, SEXP v)
{
    Rf_error("ALTSTRING classes must provide a Set_elt method");
}

static int altstring_Is_sorted_default(SEXP x) { return UNKNOWN_SORTEDNESS; }
static int altstring_No_NA_default(SEXP x) { return 0; }


/**
 ** ALTREP Initial Method Tables
 **/

static altinteger_methods_t altinteger_default_methods = {
    .UnserializeEX = altrep_UnserializeEX_default,
    .Unserialize = altrep_Unserialize_default,
    .Serialized_state = altrep_Serialized_state_default,
    .DuplicateEX = altrep_DuplicateEX_default,
    .Duplicate = altrep_Duplicate_default,
    .Coerce = altrep_Coerce_default,
    .Inspect = altrep_Inspect_default,
    .Length = altrep_Length_default,
    .Dataptr = altvec_Dataptr_default,
    .Dataptr_or_null = altvec_Dataptr_or_null_default,
    .Extract_subset = altvec_Extract_subset_default,
    .Elt = altinteger_Elt_default,
    .Get_region = altinteger_Get_region_default,
    .Is_sorted = altinteger_Is_sorted_default,
    .No_NA = altinteger_No_NA_default,
    .Sum = altinteger_Sum_default,
    .Min = altinteger_Min_default,
    .Max = altinteger_Max_default
};

static altreal_methods_t altreal_default_methods = {
    .UnserializeEX = altrep_UnserializeEX_default,
    .Unserialize = altrep_Unserialize_default,
    .Serialized_state = altrep_Serialized_state_default,
    .DuplicateEX = altrep_DuplicateEX_default,
    .Duplicate = altrep_Duplicate_default,
    .Coerce = altrep_Coerce_default,
    .Inspect = altrep_Inspect_default,
    .Length = altrep_Length_default,
    .Dataptr = altvec_Dataptr_default,
    .Dataptr_or_null = altvec_Dataptr_or_null_default,
    .Extract_subset = altvec_Extract_subset_default,
    .Elt = altreal_Elt_default,
    .Get_region = altreal_Get_region_default,
    .Is_sorted = altreal_Is_sorted_default,
    .No_NA = altreal_No_NA_default,
    .Sum = altreal_Sum_default,
    .Min = altreal_Min_default,
    .Max = altreal_Max_default
};


static altstring_methods_t altstring_default_methods = {
    .UnserializeEX = altrep_UnserializeEX_default,
    .Unserialize = altrep_Unserialize_default,
    .Serialized_state = altrep_Serialized_state_default,
    .DuplicateEX = altrep_DuplicateEX_default,
    .Duplicate = altrep_Duplicate_default,
    .Coerce = altrep_Coerce_default,
    .Inspect = altrep_Inspect_default,
    .Length = altrep_Length_default,
    .Dataptr = altvec_Dataptr_default,
    .Dataptr_or_null = altvec_Dataptr_or_null_default,
    .Extract_subset = altvec_Extract_subset_default,
    .Elt = altstring_Elt_default,
    .Set_elt = altstring_Set_elt_default,
    .Is_sorted = altstring_Is_sorted_default,
    .No_NA = altstring_No_NA_default
};


/**
 ** Class Constructors
 **/

#define INIT_CLASS(cls, type) do {				\
	*((type##_methods_t *) (CLASS_METHODS_TABLE(cls))) =	\
	    type##_default_methods;				\
    } while (FALSE)

#define MAKE_CLASS(var, type) do {				\
	var = Rf_allocVector(RAWSXP, sizeof(type##_methods_t));	\
    assert(var); \
	R_PreserveObject(var);					\
	INIT_CLASS(var, type);					\
    } while (FALSE)

R_INLINE static R_altrep_class_t R_cast_altrep_class(SEXP x)
{
    /**** some king of optional check? */
    R_altrep_class_t val = R_SUBTYPE_INIT(x);
    return val;
}

static R_altrep_class_t
make_altrep_class(SEXPTYPE type, const char *cname, const char *pname, DllInfo *dll)
{
    SEXP class_;
    switch(type) {
    case INTSXP:  MAKE_CLASS(class_, altinteger); break;
    case REALSXP: MAKE_CLASS(class_, altreal);    break;
    case STRSXP:  MAKE_CLASS(class_, altstring);  break;
    default: Rf_error("unsupported ALTREP class");
    }
    RegisterClass(class_, type, cname, pname, dll);
    return R_cast_altrep_class(class_);
}

/*  Using macros like this makes it easier to add new methods, but
    makes searching for source harder. Probably a good idea on
    balance though. */
#define DEFINE_CLASS_CONSTRUCTOR(cls, sexptype)			\
    R_altrep_class_t R_make_##cls##_class(const char *cname,	\
					  const char *pname,	\
					  DllInfo *dll)		\
    {								\
	return  make_altrep_class(sexptype, cname, pname, dll);	\
    }

DEFINE_CLASS_CONSTRUCTOR(altstring, STRSXP)
DEFINE_CLASS_CONSTRUCTOR(altinteger, INTSXP)
DEFINE_CLASS_CONSTRUCTOR(altreal, REALSXP)

static void reinit_altrep_class(SEXP class_)
{
    switch (ALTREP_CLASS_BASE_TYPE(class_)) {
    case INTSXP: INIT_CLASS(class_, altinteger); break;
    case REALSXP: INIT_CLASS(class_, altreal); break;
    case STRSXP: INIT_CLASS(class_, altstring); break;
    default: Rf_error("unsupported ALTREP class_");
    }
}


/**
 ** ALTREP Method Setters
 **/

#define DEFINE_METHOD_SETTER(CNAME, MNAME)				\
    void R_set_##CNAME##_##MNAME##_method(R_altrep_class_t cls,		\
					  R_##CNAME##_##MNAME##_method_t fun) \
    {									\
	CNAME##_methods_t *m = static_cast<CNAME##_methods_t *>(CLASS_METHODS_TABLE(R_SEXP(cls)));	\
	m->MNAME = fun;							\
    }

DEFINE_METHOD_SETTER(altrep, UnserializeEX)
DEFINE_METHOD_SETTER(altrep, Unserialize)
DEFINE_METHOD_SETTER(altrep, Serialized_state)
DEFINE_METHOD_SETTER(altrep, DuplicateEX)
DEFINE_METHOD_SETTER(altrep, Duplicate)
DEFINE_METHOD_SETTER(altrep, Coerce)
DEFINE_METHOD_SETTER(altrep, Inspect)
DEFINE_METHOD_SETTER(altrep, Length)

DEFINE_METHOD_SETTER(altvec, Dataptr)
DEFINE_METHOD_SETTER(altvec, Dataptr_or_null)
DEFINE_METHOD_SETTER(altvec, Extract_subset)

DEFINE_METHOD_SETTER(altinteger, Elt)
DEFINE_METHOD_SETTER(altinteger, Get_region)
DEFINE_METHOD_SETTER(altinteger, Is_sorted)
DEFINE_METHOD_SETTER(altinteger, No_NA)
DEFINE_METHOD_SETTER(altinteger, Sum)
DEFINE_METHOD_SETTER(altinteger, Min)
DEFINE_METHOD_SETTER(altinteger, Max)

DEFINE_METHOD_SETTER(altreal, Elt)
DEFINE_METHOD_SETTER(altreal, Get_region)
DEFINE_METHOD_SETTER(altreal, Is_sorted)
DEFINE_METHOD_SETTER(altreal, No_NA)
DEFINE_METHOD_SETTER(altreal, Sum)
DEFINE_METHOD_SETTER(altreal, Min)
DEFINE_METHOD_SETTER(altreal, Max)

DEFINE_METHOD_SETTER(altstring, Elt)
DEFINE_METHOD_SETTER(altstring, Set_elt)
DEFINE_METHOD_SETTER(altstring, Is_sorted)
DEFINE_METHOD_SETTER(altstring, No_NA)


/**
 ** ALTREP Object Constructor and Utility Functions
 **/

SEXP R_new_altrep(R_altrep_class_t aclass, SEXP data1, SEXP data2)
{
    SEXP sclass = R_SEXP(aclass);
    SEXPTYPE type = ALTREP_CLASS_BASE_TYPE(sclass);
    SEXP ans = Rf_cons(data1, data2);
    SET_TYPEOF(ans, type);
    SET_ALTREP_CLASS(ans, sclass);
    return ans;
}

Rboolean R_altrep_inherits(SEXP x, R_altrep_class_t class_)
{
    return Rboolean(ALTREP(x) && ALTREP_CLASS(x) == R_SEXP(class_));
}


/**
 ** Compact Integer Sequences
 **/

/*
 * Methods
 */

#define COMPACT_SEQ_INFO(x) R_altrep_data1(x)
#define COMPACT_SEQ_EXPANDED(x) R_altrep_data2(x)
#define SET_COMPACT_SEQ_EXPANDED(x, v) R_set_altrep_data2(x, v)

/* needed for now for objects serialized with INTSXP state */
#define COMPACT_INTSEQ_SERIALIZED_STATE_LENGTH(info) \
    (TYPEOF(info) == INTSXP ? INTEGER0(info)[0] : (R_xlen_t) REAL0(info)[0])
#define COMPACT_INTSEQ_SERIALIZED_STATE_FIRST(info) \
    (TYPEOF(info) == INTSXP ? INTEGER0(info)[1] : (int) REAL0(info)[1])
#define COMPACT_INTSEQ_SERIALIZED_STATE_INCR(info) \
    (TYPEOF(info) == INTSXP ? INTEGER0(info)[2] : (int) REAL0(info)[2])

/* info is stored as REALSXP to allow for long vector length */
#define COMPACT_INTSEQ_INFO_LENGTH(info) ((R_xlen_t) REAL0(info)[0])
#define COMPACT_INTSEQ_INFO_FIRST(info) ((int) REAL0(info)[1])
#define COMPACT_INTSEQ_INFO_INCR(info) ((int) REAL0(info)[2])

/* By default, compact integer sequences are marked as not mutable at
   creation time. Thus even when expanded the expanded data will
   correspond to the original integer sequence (unless it runs into
   mis-behaving C code). If COMPACT_INTSEQ_MUTABLE is defined, then
   the sequence is not marked as not mutable. Once the DATAPTR has
   been requested and releases, the expanded data might be modified by
   an assignment and no longer correspond to the original sequence. */
//#define COMPACT_INTSEQ_MUTABLE

static SEXP compact_intseq_Serialized_state(SEXP x)
{
#ifdef COMPACT_INTSEQ_MUTABLE
    /* This drops through to standard serialization for expanded
       compact vectors */
    if (COMPACT_SEQ_EXPANDED(x) != nullptr)
	return nullptr;
#endif
    return COMPACT_SEQ_INFO(x);
}

static SEXP new_compact_intseq(R_xlen_t, int, int);
static SEXP new_compact_realseq(R_xlen_t, double, double);

static SEXP compact_intseq_Unserialize(SEXP class_, SEXP state)
{
    R_xlen_t n = COMPACT_INTSEQ_SERIALIZED_STATE_LENGTH(state);
    int n1 = COMPACT_INTSEQ_SERIALIZED_STATE_FIRST(state);
    int inc = COMPACT_INTSEQ_SERIALIZED_STATE_INCR(state);

    if (inc == 1)
	return new_compact_intseq(n, n1,  1);
    else if (inc == -1)
	return new_compact_intseq(n, n1,  -1);
    else
	Rf_error("compact sequences with increment %d not supported yet", inc);
}

static SEXP compact_intseq_Coerce(SEXP x, int type)
{
#ifdef COMPACT_INTSEQ_MUTABLE
    /* This drops through to standard coercion for expanded compact
       vectors */
    if (COMPACT_SEQ_EXPANDED(x) != nullptr)
	return nullptr;
#endif
    if (type == REALSXP) {
	SEXP info = COMPACT_SEQ_INFO(x);
	R_xlen_t n = COMPACT_INTSEQ_INFO_LENGTH(info);
	int n1 = COMPACT_INTSEQ_INFO_FIRST(info);
	int inc = COMPACT_INTSEQ_INFO_INCR(info);
	return new_compact_realseq(n, n1, inc);
    }
    else return nullptr;
}

static SEXP compact_intseq_Duplicate(SEXP x, Rboolean deep)
{
    R_xlen_t n = XLENGTH(x);
    SEXP val = Rf_allocVector(INTSXP, n);
    INTEGER_GET_REGION(x, 0, n, INTEGER0(val));
    return val;
}

static
Rboolean compact_intseq_Inspect(SEXP x, int pre, int deep, int pvec,
				void (*inspect_subtree)(SEXP, int, int, int))
{
    int inc = COMPACT_INTSEQ_INFO_INCR(COMPACT_SEQ_INFO(x));
    if (inc != 1 && inc != -1)
	Rf_error("compact sequences with increment %d not supported yet", inc);

#ifdef COMPACT_INTSEQ_MUTABLE
    if (COMPACT_SEQ_EXPANDED(x) != nullptr) {
	Rprintf("  <expanded compact integer sequence>\n");
	inspect_subtree(COMPACT_SEQ_EXPANDED(x), pre, deep, pvec);
	return TRUE;
    }
#endif

    int n = LENGTH(x);
    int n1 = INTEGER_ELT(x, 0);
    int n2 = inc == 1 ? n1 + n - 1 : n1 - n + 1;
    Rprintf(" %d : %d (%s)", n1, n2,
	    COMPACT_SEQ_EXPANDED(x) == nullptr ? "compact" : "expanded");
    Rprintf("\n");
    return TRUE;
}

R_INLINE static R_xlen_t compact_intseq_Length(SEXP x)
{
    SEXP info = COMPACT_SEQ_INFO(x);
    return COMPACT_INTSEQ_INFO_LENGTH(info);
}

static void *compact_intseq_Dataptr(SEXP x, Rboolean writeable)
{
    if (COMPACT_SEQ_EXPANDED(x) == nullptr) {
	/* no need to re-run if expanded data exists */
	PROTECT(x);
	SEXP info = COMPACT_SEQ_INFO(x);
	R_xlen_t n = COMPACT_INTSEQ_INFO_LENGTH(info);
	int n1 = COMPACT_INTSEQ_INFO_FIRST(info);
	int inc = COMPACT_INTSEQ_INFO_INCR(info);
	SEXP val = Rf_allocVector(INTSXP, n);
	int *data = INTEGER(val);

	if (inc == 1) {
	    /* compact sequences n1 : n2 with n1 <= n2 */
	    for (int i = 0; i < n; i++)
		data[i] = n1 + i;
	}
	else if (inc == -1) {
	    /* compact sequences n1 : n2 with n1 > n2 */
	    for (int i = 0; i < n; i++)
		data[i] = n1 - i;
	}
	else
	    Rf_error("compact sequences with increment %d not supported yet", inc);

	SET_COMPACT_SEQ_EXPANDED(x, val);
	UNPROTECT(1);
    }
    return DATAPTR(COMPACT_SEQ_EXPANDED(x));
}

static const void *compact_intseq_Dataptr_or_null(SEXP x)
{
    SEXP val = COMPACT_SEQ_EXPANDED(x);
    return val == nullptr ? nullptr : DATAPTR(val);
}

static int compact_intseq_Elt(SEXP x, R_xlen_t i)
{
    SEXP ex = COMPACT_SEQ_EXPANDED(x);
    if (ex != nullptr)
	return INTEGER0(ex)[i];
    else {
	SEXP info = COMPACT_SEQ_INFO(x);
	int n1 = COMPACT_INTSEQ_INFO_FIRST(info);
	int inc = COMPACT_INTSEQ_INFO_INCR(info);
	return (int) (n1 + inc * i);
    }
}

static R_xlen_t
compact_intseq_Get_region(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf)
{
    /* should not get here if x is already expanded */
    CHECK_NOT_EXPANDED(sx);

    SEXP info = COMPACT_SEQ_INFO(sx);
    R_xlen_t size = COMPACT_INTSEQ_INFO_LENGTH(info);
    R_xlen_t n1 = COMPACT_INTSEQ_INFO_FIRST(info);
    int inc = COMPACT_INTSEQ_INFO_INCR(info);

    R_xlen_t ncopy = size - i > n ? n : size - i;
    if (inc == 1) {
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = (int) (n1 + k + i);
	return ncopy;
    }
    else if (inc == -1) {
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = (int) (n1 - k - i);
	return ncopy;
    }
    else
	Rf_error("compact sequences with increment %d not supported yet", inc);
}

static int compact_intseq_Is_sorted(SEXP x)
{
#ifdef COMPACT_INTSEQ_MUTABLE
    /* If the vector has been expanded it may have been modified. */
    if (COMPACT_SEQ_EXPANDED(x) != nullptr)
	return UNKNOWN_SORTEDNESS;
#endif
    int inc = COMPACT_INTSEQ_INFO_INCR(COMPACT_SEQ_INFO(x));
    return inc < 0 ? SORTED_DECR : SORTED_INCR;
}

static int compact_intseq_No_NA(SEXP x)
{
#ifdef COMPACT_INTSEQ_MUTABLE
    /* If the vector has been expanded it may have been modified. */
    if (COMPACT_SEQ_EXPANDED(x) != nullptr)
	return FALSE;
#endif
    return TRUE;
}

/* XXX this also appears in summary.c. move to header file?*/
#define R_INT_MIN (1 + INT_MIN)

static SEXP compact_intseq_Sum(SEXP x, Rboolean narm)
{
#ifdef COMPACT_INTSEQ_MUTABLE
    /* If the vector has been expanded it may have been modified. */
    if (COMPACT_SEQ_EXPANDED(x) != nullptr) 
	return nullptr;
#endif
    double tmp;
    SEXP info = COMPACT_SEQ_INFO(x);
    R_xlen_t size = COMPACT_INTSEQ_INFO_LENGTH(info);
    R_xlen_t n1 = COMPACT_INTSEQ_INFO_FIRST(info);
    int inc = COMPACT_INTSEQ_INFO_INCR(info);
    tmp = (size / 2.0) * (n1 + n1 + inc * (size - 1));
    if(tmp > INT_MAX || tmp < R_INT_MIN)
	/**** check for overflow of exact integer range? */
	return Rf_ScalarReal(tmp);
    else
	return Rf_ScalarInteger((int) tmp);
}


/*
 * Class Objects and Method Tables
 */

R_altrep_class_t R_compact_intseq_class;

static void InitCompactIntegerClass()
{
    R_altrep_class_t cls = R_make_altinteger_class("compact_intseq", "base", nullptr);
    R_compact_intseq_class = cls;

    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, compact_intseq_Unserialize);
    R_set_altrep_Serialized_state_method(cls, compact_intseq_Serialized_state);
    R_set_altrep_Duplicate_method(cls, compact_intseq_Duplicate);
    R_set_altrep_Coerce_method(cls, compact_intseq_Coerce);
    R_set_altrep_Inspect_method(cls, compact_intseq_Inspect);
    R_set_altrep_Length_method(cls, compact_intseq_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, compact_intseq_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, compact_intseq_Dataptr_or_null);

    /* override ALTINTEGER methods */
    R_set_altinteger_Elt_method(cls, compact_intseq_Elt);
    R_set_altinteger_Get_region_method(cls, compact_intseq_Get_region);
    R_set_altinteger_Is_sorted_method(cls, compact_intseq_Is_sorted);
    R_set_altinteger_No_NA_method(cls, compact_intseq_No_NA);
    R_set_altinteger_Sum_method(cls, compact_intseq_Sum);
}


/*
 * Constructor
 */

static SEXP new_compact_intseq(R_xlen_t n, int n1, int inc)
{
    if (n == 1) return Rf_ScalarInteger(n1);

    if (inc != 1 && inc != -1)
	Rf_error("compact sequences with increment %d not supported yet", inc);

    /* info used REALSXP to allow for long vectors */
    SEXP info = Rf_allocVector(REALSXP, 3);
    REAL0(info)[0] = (double) n;
    REAL0(info)[1] = (double) n1;
    REAL0(info)[2] = (double) inc;

    SEXP ans = R_new_altrep(R_compact_intseq_class, info, nullptr);
#ifndef COMPACT_INTSEQ_MUTABLE
    MARK_NOT_MUTABLE(ans); /* force duplicate on modify */
#endif

    return ans;
}


/**
 ** Compact Real Sequences
 **/

/*
 * Methods
 */

#define COMPACT_REALSEQ_INFO_LENGTH(info) ((R_xlen_t) REAL0(info)[0])
#define COMPACT_REALSEQ_INFO_FIRST(info) REAL0(info)[1]
#define COMPACT_REALSEQ_INFO_INCR(info) REAL0(info)[2]

static SEXP compact_realseq_Serialized_state(SEXP x)
{
    return COMPACT_SEQ_INFO(x);
}

static SEXP compact_realseq_Unserialize(SEXP class_, SEXP state)
{
    double inc = COMPACT_REALSEQ_INFO_INCR(state);
    R_xlen_t len = COMPACT_REALSEQ_INFO_LENGTH(state);
    double n1 = COMPACT_REALSEQ_INFO_FIRST(state);

    if (inc == 1)
	return new_compact_realseq(len, n1,  1);
    else if (inc == -1)
	return new_compact_realseq(len, n1, -1);
    else
	Rf_error("compact sequences with increment %f not supported yet", inc);
}

static SEXP compact_realseq_Duplicate(SEXP x, Rboolean deep)
{
    R_xlen_t n = XLENGTH(x);
    SEXP val = Rf_allocVector(REALSXP, n);
    REAL_GET_REGION(x, 0, n, REAL0(val));
    return val;
}

static
Rboolean compact_realseq_Inspect(SEXP x, int pre, int deep, int pvec,
				 void (*inspect_subtree)(SEXP, int, int, int))
{
    double inc = COMPACT_REALSEQ_INFO_INCR(COMPACT_SEQ_INFO(x));
    if (inc != 1 && inc != -1)
	Rf_error("compact sequences with increment %f not supported yet", inc);

    R_xlen_t n = XLENGTH(x);
    R_xlen_t n1 = (R_xlen_t) REAL_ELT(x, 0);
    R_xlen_t n2 = inc == 1 ? n1 + n - 1 : n1 - n + 1;
    Rprintf(" %ld : %ld (%s)", n1, n2,
	    COMPACT_SEQ_EXPANDED(x) == nullptr ? "compact" : "expanded");
    Rprintf("\n");
    return TRUE;
}

R_INLINE static R_xlen_t compact_realseq_Length(SEXP x)
{
    return (R_xlen_t) REAL0(COMPACT_SEQ_INFO(x))[0];
}

static void *compact_realseq_Dataptr(SEXP x, Rboolean writeable)
{
    if (COMPACT_SEQ_EXPANDED(x) == nullptr) {
	PROTECT(x);
	SEXP info = COMPACT_SEQ_INFO(x);
	R_xlen_t n = COMPACT_REALSEQ_INFO_LENGTH(info);
	double n1 = COMPACT_REALSEQ_INFO_FIRST(info);
	double inc = COMPACT_REALSEQ_INFO_INCR(info);

	SEXP val = Rf_allocVector(REALSXP, (R_xlen_t) n);
	double *data = REAL(val);

	if (inc == 1) {
	    /* compact sequences n1 : n2 with n1 <= n2 */
	    for (R_xlen_t i = 0; i < n; i++)
		data[i] = n1 + i;
	}
	else if (inc == -1) {
	    /* compact sequences n1 : n2 with n1 > n2 */
	    for (R_xlen_t i = 0; i < n; i++)
		data[i] = n1 - i;
	}
	else
	    Rf_error("compact sequences with increment %f not supported yet", inc);

	SET_COMPACT_SEQ_EXPANDED(x, val);
	UNPROTECT(1);
    }
    return DATAPTR(COMPACT_SEQ_EXPANDED(x));
}

static const void *compact_realseq_Dataptr_or_null(SEXP x)
{
    SEXP val = COMPACT_SEQ_EXPANDED(x);
    return val == nullptr ? nullptr : DATAPTR(val);
}

static double compact_realseq_Elt(SEXP x, R_xlen_t i)
{
    SEXP ex = COMPACT_SEQ_EXPANDED(x);
    if (ex != nullptr)
	return REAL0(ex)[i];
    else {
	SEXP info = COMPACT_SEQ_INFO(x);
	double n1 = COMPACT_REALSEQ_INFO_FIRST(info);
	double inc = COMPACT_REALSEQ_INFO_INCR(info);
	return n1 + inc * i;
    }
}

static R_xlen_t
compact_realseq_Get_region(SEXP sx, R_xlen_t i, R_xlen_t n, double *buf)
{
    /* should not get here if x is already expanded */
    CHECK_NOT_EXPANDED(sx);

    SEXP info = COMPACT_SEQ_INFO(sx);
    R_xlen_t size = COMPACT_REALSEQ_INFO_LENGTH(info);
    double n1 = COMPACT_REALSEQ_INFO_FIRST(info);
    double inc = COMPACT_REALSEQ_INFO_INCR(info);

    R_xlen_t ncopy = size - i > n ? n : size - i;
    if (inc == 1) {
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = n1 + k + i;
	return ncopy;
    }
    else if (inc == -1) {
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = n1 - k - i;
	return ncopy;
    }
    else
	Rf_error("compact sequences with increment %f not supported yet", inc);
}

static int compact_realseq_Is_sorted(SEXP x)
{
#ifdef COMPACT_REALSEQ_MUTABLE
    /* If the vector has been expanded it may have been modified. */
    if (COMPACT_SEQ_EXPANDED(x) != nullptr)
	return UNKNOWN_SORTEDNESS;
#endif
    double inc = COMPACT_REALSEQ_INFO_INCR(COMPACT_SEQ_INFO(x));
    return inc < 0 ? SORTED_DECR : SORTED_INCR;
}

static int compact_realseq_No_NA(SEXP x)
{
#ifdef COMPACT_REALSEQ_MUTABLE
    /* If the vector has been expanded it may have been modified. */
    if (COMPACT_SEQ_EXPANDED(x) != nullptr)
	return FALSE;
#endif
    return TRUE;
}

static SEXP compact_realseq_Sum(SEXP x, Rboolean narm)
{
#ifdef COMPACT_INTSEQ_MUTABLE
    /* If the vector has been expanded it may have been modified. */
    if (COMPACT_SEQ_EXPANDED(x) != nullptr) 
	return nullptr;
#endif
    SEXP info = COMPACT_SEQ_INFO(x);
    double size = (double) COMPACT_REALSEQ_INFO_LENGTH(info);
    double n1 = COMPACT_REALSEQ_INFO_FIRST(info);
    double inc = COMPACT_REALSEQ_INFO_INCR(info);
    return Rf_ScalarReal((size / 2.0) *(n1 + n1 + inc * (size - 1)));
}


/*
 * Class Objects and Method Tables
 */


R_altrep_class_t R_compact_realseq_class;

static void InitCompactRealClass()
{
    R_altrep_class_t cls = R_make_altreal_class("compact_realseq", "base",
						nullptr);
    R_compact_realseq_class = cls;

    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, compact_realseq_Unserialize);
    R_set_altrep_Serialized_state_method(cls, compact_realseq_Serialized_state);
    R_set_altrep_Duplicate_method(cls, compact_realseq_Duplicate);
    R_set_altrep_Inspect_method(cls, compact_realseq_Inspect);
    R_set_altrep_Length_method(cls, compact_realseq_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, compact_realseq_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, compact_realseq_Dataptr_or_null);

    /* override ALTREAL methods */
    R_set_altreal_Elt_method(cls, compact_realseq_Elt);
    R_set_altreal_Get_region_method(cls, compact_realseq_Get_region);
    R_set_altreal_Is_sorted_method(cls, compact_realseq_Is_sorted);
    R_set_altreal_No_NA_method(cls, compact_realseq_No_NA);
    R_set_altreal_Sum_method(cls, compact_realseq_Sum);
}


/*
 * Constructor
 */

static SEXP new_compact_realseq(R_xlen_t n, double n1, double inc)
{
    if (n == 1) return Rf_ScalarReal(n1);

    if (inc != 1 && inc != -1)
	Rf_error("compact sequences with increment %f not supported yet", inc);

    SEXP info = Rf_allocVector(REALSXP, 3);
    REAL(info)[0] = n;
    REAL(info)[1] = n1;
    REAL(info)[2] = inc;

    SEXP ans = R_new_altrep(R_compact_realseq_class, info, nullptr);
    MARK_NOT_MUTABLE(ans); /* force duplicate on modify */

    return ans;
}


/**
 ** Compact Integer/Real Sequences
 **/

HIDDEN SEXP R_compact_intrange(R_xlen_t n1, R_xlen_t n2)
{
    R_xlen_t n = std::abs(n2 - n1) + 1;

    if (n1 <= INT_MIN || n1 > INT_MAX || n2 <= INT_MIN || n2 > INT_MAX)
	return new_compact_realseq(n, n1, n1 <= n2 ? 1 : -1);
    else
	return new_compact_intseq(n, (int) n1, n1 <= n2 ? 1 : -1);
}


/**
 ** Deferred String Coercions
 **/

/*
 * Methods
 */

#define DEFERRED_STRING_STATE(x) R_altrep_data1(x)
#define	CLEAR_DEFERRED_STRING_STATE(x) R_set_altrep_data1(x, nullptr)
#define DEFERRED_STRING_EXPANDED(x) R_altrep_data2(x)
#define SET_DEFERRED_STRING_EXPANDED(x, v) R_set_altrep_data2(x, v)

#define MAKE_DEFERRED_STRING_STATE(v, sp) CONS(v, sp)
#define DEFERRED_STRING_STATE_ARG(s) CAR(s)
#define DEFERRED_STRING_STATE_SCIPEN(s) CDR(s)

#define DEFERRED_STRING_ARG(x) \
    DEFERRED_STRING_STATE_ARG(DEFERRED_STRING_STATE(x))
#define DEFERRED_STRING_SCIPEN(x) \
    DEFERRED_STRING_STATE_SCIPEN(DEFERRED_STRING_STATE(x))

static SEXP deferred_string_Serialized_state(SEXP x)
{
    /* This drops through to standard serialization for fully expanded
       deferred string conversions. Partial expansions are OK since
       they still correspond to the original data. An assignment to
       the object will access the DATAPTR and force a full expansion
       and dropping the original data. */
    SEXP state = DEFERRED_STRING_STATE(x);
    return state != nullptr ? state : nullptr;
}

static SEXP deferred_string_Unserialize(SEXP class_, SEXP state)
{
    SEXP arg = DEFERRED_STRING_STATE_ARG(state);
    SEXP sp = DEFERRED_STRING_STATE_SCIPEN(state);
    return R_deferred_coerceToString(arg, sp);
}

static
Rboolean deferred_string_Inspect(SEXP x, int pre, int deep, int pvec,
				 void (*inspect_subtree)(SEXP, int, int, int))
{
    SEXP state = DEFERRED_STRING_STATE(x);
    if (state != nullptr) {
	SEXP arg = DEFERRED_STRING_STATE_ARG(state);
	Rprintf("  <deferred string conversion>\n");
	inspect_subtree(arg, pre, deep, pvec);
    }
    else {
	Rprintf("  <expanded string conversion>\n");
	inspect_subtree(DEFERRED_STRING_EXPANDED(x), pre, deep, pvec);
    }
    return TRUE;
}

R_INLINE static R_xlen_t deferred_string_Length(SEXP x)
{
    SEXP state = DEFERRED_STRING_STATE(x);
    return state == nullptr ?
	XLENGTH(DEFERRED_STRING_EXPANDED(x)) :
	XLENGTH(DEFERRED_STRING_STATE_ARG(state));
}

R_INLINE static SEXP ExpandDeferredStringElt(SEXP x, R_xlen_t i)
{
    /* make sure the STRSXP for the expanded string is allocated */
    /* not yet expanded strings are nullptr in the STRSXP */
    SEXP val = DEFERRED_STRING_EXPANDED(x);
    if (val == nullptr) {
	R_xlen_t n = XLENGTH(x);
	val = Rf_allocVector(STRSXP, n);
	memset(STDVEC_DATAPTR(val), 0, n * sizeof(SEXP));
	SET_DEFERRED_STRING_EXPANDED(x, val);
    }

    SEXP elt = STRING_ELT(val, i);
    if (elt == nullptr) {
	int warn; /* not used by the coercion functions */
	int savedigits, savescipen;
	SEXP data = DEFERRED_STRING_ARG(x);
	switch(TYPEOF(data)) {
	case INTSXP:
	    elt = Rf_StringFromInteger(INTEGER_ELT(data, i), &warn);
	    break;
	case REALSXP:
	    savedigits = R_print.digits;
	    savescipen = R_print.scipen;
	    R_print.digits = DBL_DIG;/* MAX precision */
	    R_print.scipen = INTEGER0(DEFERRED_STRING_SCIPEN(x))[0];
	    elt = Rf_StringFromReal(REAL_ELT(data, i), &warn);
	    R_print.digits = savedigits;
	    R_print.scipen = savescipen;
	    break;
	default:
	    Rf_error("unsupported type for deferred string coercion");
	}
	SET_STRING_ELT(val, i, elt);
    }
    return elt;
}

R_INLINE static void expand_deferred_string(SEXP x)
{
    SEXP state = DEFERRED_STRING_STATE(x);
    if (state != nullptr) {
	/* expanded data may be incomplete until original data is removed */
	PROTECT(x);
	R_xlen_t n = XLENGTH(x), i;
	if (n == 0)
	    SET_DEFERRED_STRING_EXPANDED(x, Rf_allocVector(STRSXP, 0));
	else
	    for (i = 0; i < n; i++)
		ExpandDeferredStringElt(x, i);
	CLEAR_DEFERRED_STRING_STATE(x); /* allow arg to be reclaimed */
	UNPROTECT(1);
    }
}

static void *deferred_string_Dataptr(SEXP x, Rboolean writeable)
{
    expand_deferred_string(x);
    return DATAPTR(DEFERRED_STRING_EXPANDED(x));
}

static const void *deferred_string_Dataptr_or_null(SEXP x)
{
    SEXP state = DEFERRED_STRING_STATE(x);
    return state != nullptr ? nullptr : DATAPTR(DEFERRED_STRING_EXPANDED(x));
}

static SEXP deferred_string_Elt(SEXP x, R_xlen_t i)
{
    SEXP state = DEFERRED_STRING_STATE(x);
    if (state == nullptr)
	/* string is fully expanded */
	return STRING_ELT(DEFERRED_STRING_EXPANDED(x), i);
    else {
	/* expand only the requested element */
	PROTECT(x);
	SEXP elt = ExpandDeferredStringElt(x, i);
	UNPROTECT(1);
	return elt;
    }
}

static void deferred_string_Set_elt(SEXP x, R_xlen_t i, SEXP v)
{
    expand_deferred_string(x);
    SET_STRING_ELT(DEFERRED_STRING_EXPANDED(x), i, v);
}

static int deferred_string_Is_sorted(SEXP x)
{
    SEXP state = DEFERRED_STRING_STATE(x);
    if (state == nullptr)
	/* string is fully expanded and may have been modified. */
	return UNKNOWN_SORTEDNESS;
    else {
	/* defer to the argument */
	SEXP arg = DEFERRED_STRING_STATE_ARG(state);
	switch(TYPEOF(arg)) {
	case INTSXP: return INTEGER_IS_SORTED(arg);
	case REALSXP: return REAL_IS_SORTED(arg);
	default: return UNKNOWN_SORTEDNESS;
	}
    }
}

static int deferred_string_No_NA(SEXP x)
{
    SEXP state = DEFERRED_STRING_STATE(x);
    if (state == nullptr)
	/* string is fully expanded and may have been modified. */
	return FALSE;
    else {
	/* defer to the argument */
	SEXP arg = DEFERRED_STRING_STATE_ARG(state);
	switch(TYPEOF(arg)) {
	case INTSXP: return INTEGER_NO_NA(arg);
	case REALSXP: return REAL_NO_NA(arg);
	default: return FALSE;
	}
    }
}

static SEXP deferred_string_Extract_subset(SEXP x, SEXP indx, SEXP call)
{
    SEXP result = nullptr;

    if (! OBJECT(x) && ATTRIB(x) == nullptr &&
	DEFERRED_STRING_STATE(x) != nullptr) {
	/* For deferred string coercions, create a new conversion
	   using the subset of the argument.  Could try to
	   preserve/share coercions already done, if there are any. */
	SEXP data = DEFERRED_STRING_ARG(x);
	SEXP sp = DEFERRED_STRING_SCIPEN(x);
	PROTECT(result = Rf_ExtractSubset(data, indx, call));
	result = R_deferred_coerceToString(result, sp);
	UNPROTECT(1);
	return result;
    }

    return result;
}


/*
 * Class Object and Method Table
 */

static R_altrep_class_t R_deferred_string_class;

static void InitDefferredStringClass()
{
    R_altrep_class_t cls = R_make_altstring_class("deferred_string", "base", nullptr);
    R_deferred_string_class = cls;

    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, deferred_string_Unserialize);
    R_set_altrep_Serialized_state_method(cls, deferred_string_Serialized_state);
    R_set_altrep_Inspect_method(cls, deferred_string_Inspect);
    R_set_altrep_Length_method(cls, deferred_string_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, deferred_string_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, deferred_string_Dataptr_or_null);
    R_set_altvec_Extract_subset_method(cls, deferred_string_Extract_subset);

    /* override ALTSTRING methods */
    R_set_altstring_Elt_method(cls, deferred_string_Elt);
    R_set_altstring_Set_elt_method(cls, deferred_string_Set_elt);
    R_set_altstring_Is_sorted_method(cls, deferred_string_Is_sorted);
    R_set_altstring_No_NA_method(cls, deferred_string_No_NA);
}


/*
 * Constructor
 */

HIDDEN SEXP R_deferred_coerceToString(SEXP v, SEXP sp)
{
    SEXP ans = nullptr;
    switch (TYPEOF(v)) {
    case INTSXP:
    case REALSXP:
	if (sp == nullptr)
	    sp = Rf_ScalarInteger(R_print.scipen);
	MARK_NOT_MUTABLE(v); /* make sure it can't change once captured */
	ans = PROTECT(MAKE_DEFERRED_STRING_STATE(v, sp));
	ans = R_new_altrep(R_deferred_string_class, ans, nullptr);
	UNPROTECT(1); /* ans */
	break;
    default:
	Rf_error("unsupported type for deferred string coercion");
    }
    return ans;
}


/**
 ** Memory Mapped Vectors
 **/

/* For now, this code is designed to work both in base R and in a
   package. Some simplifications would be possible if it was only to
   be used in base. in particular, the issue of finalizing objects
   before unloading the library would not need to be addressed, and
   ordinary finalizers in the external pointers could be used instead
   of maintaining a weak reference list of the live mmap objects. */

/*
 * MMAP Object State
 */

/* State is held in a LISTSXP of length 3, and includes

       file
       size and length in a REALSXP
       type, ptrOK, wrtOK, serOK in an INTSXP

   These are used by the methods, and also represent the serialized
   state object.
 */

static SEXP make_mmap_state(SEXP file, size_t size, SEXPTYPE type,
			    Rboolean ptrOK, Rboolean wrtOK, Rboolean serOK)
{
    SEXP sizes = PROTECT(Rf_allocVector(REALSXP, 2));
    double *dsizes = REAL(sizes);
    dsizes[0] = size;
    switch(type) {
    case INTSXP: dsizes[1] = size / sizeof(int); break;
    case REALSXP: dsizes[1] = size / sizeof(double); break;
    default: Rf_error("mmap for %s not supported yet", Rf_type2char(SEXPTYPE(type)));
    }

    SEXP info = PROTECT(Rf_allocVector(INTSXP, 4));
    INTEGER(info)[0] = type;
    INTEGER(info)[1] = ptrOK;
    INTEGER(info)[2] = wrtOK;
    INTEGER(info)[3] = serOK;

    SEXP state = Rf_list3(file, sizes, info);

    UNPROTECT(2);
    return state;
}

#define MMAP_STATE_FILE(x) CAR(x)
#define MMAP_STATE_SIZE(x) ((size_t) REAL_ELT(CADR(x), 0))
#define MMAP_STATE_LENGTH(x) ((size_t) REAL_ELT(CADR(x), 1))
#define MMAP_STATE_TYPE(x) INTEGER(CADDR(x))[0]
#define MMAP_STATE_PTROK(x) INTEGER(CADDR(x))[1]
#define MMAP_STATE_WRTOK(x) INTEGER(CADDR(x))[2]
#define MMAP_STATE_SEROK(x) INTEGER(CADDR(x))[3]


/*
 * MMAP Classes and Objects
 */

static R_altrep_class_t mmap_integer_class;
static R_altrep_class_t mmap_real_class;

/* MMAP objects are ALTREP objects with data fields

       data1: an external pointer to the mmaped address
       data2: the MMAP object's state

   The state is also stored in the Protected field of the external
   pointer for use by the finalizer.
*/

static void register_mmap_eptr(SEXP eptr);
static SEXP make_mmap(void *p, SEXP file, size_t size, SEXPTYPE type,
		      Rboolean ptrOK, Rboolean wrtOK, Rboolean serOK)
{
    SEXP state = PROTECT(make_mmap_state(file, size,
					 type, ptrOK, wrtOK, serOK));
    SEXP eptr = PROTECT(R_MakeExternalPtr(p, nullptr, state));
    register_mmap_eptr(eptr);

    R_altrep_class_t class_;
    switch(type) {
    case INTSXP:
	class_ = mmap_integer_class;
	break;
    case REALSXP:
	class_ = mmap_real_class;
	break;
    default: Rf_error("mmap for %s not supported yet", Rf_type2char(type));
    }

    SEXP ans = R_new_altrep(class_, eptr, state);
    if (ptrOK && ! wrtOK)
	MARK_NOT_MUTABLE(ans);

    UNPROTECT(2); /* state, eptr */
    return ans;
}

#define MMAP_EPTR(x) R_altrep_data1(x)
#define MMAP_STATE(x) R_altrep_data2(x)
#define MMAP_LENGTH(x) MMAP_STATE_LENGTH(MMAP_STATE(x))
#define MMAP_PTROK(x) MMAP_STATE_PTROK(MMAP_STATE(x))
#define MMAP_WRTOK(x) MMAP_STATE_WRTOK(MMAP_STATE(x))
#define MMAP_SEROK(x) MMAP_STATE_SEROK(MMAP_STATE(x))

#define MMAP_EPTR_STATE(x) R_ExternalPtrProtected(x)

R_INLINE static void* MMAP_ADDR(SEXP x)
{
    SEXP eptr = MMAP_EPTR(x);
    void* addr = R_ExternalPtrAddr(eptr);

    if (addr == nullptr)
	Rf_error("object has been unmapped");
    return addr;
}

/* We need to maintain a list of weak references to the external
   pointers of memory-mapped objects so a request to unload the shared
   library can finalize them before unloading; otherwise, attempting
   to run a finalizer after unloading would result in an illegal
   instruction. */

static SEXP mmap_list = nullptr;

#define MAXCOUNT 10

static void mmap_finalize(SEXP eptr);
static void register_mmap_eptr(SEXP eptr)
{
    if (mmap_list == nullptr) {
	mmap_list = PairList::cons(nullptr, nullptr);
	R_PreserveObject(mmap_list);
    }

    /* clean out the weak list every MAXCOUNT calls*/
    static int cleancount = MAXCOUNT;
    if (--cleancount <= 0) {
	cleancount = MAXCOUNT;
	for (SEXP last = mmap_list, next = CDR(mmap_list);
	     next != nullptr;
	     next = CDR(next))
	    if (R_WeakRefKey(CAR(next)) == nullptr)
		SETCDR(last, CDR(next));
	    else
		last = next;
    }

    /* add a weak reference with a finalizer to the list */
    SETCDR(mmap_list, 
	   CONS(R_MakeWeakRefC(eptr, nullptr, mmap_finalize, TRUE),
		CDR(mmap_list)));

    /* store the weak reference in the external pointer for do_munmap_file */
    R_SetExternalPtrTag(eptr, CAR(CDR(mmap_list)));
}

#ifdef SIMPLEMMAP
static void finalize_mmap_objects()
{
    if (mmap_list == nullptr)
	return;

    /* finalize any remaining mmap objects before unloading */
    for (SEXP next = CDR(mmap_list); next != nullptr; next = CDR(next))
	R_RunWeakRefFinalizer(CAR(next));
    R_ReleaseObject(mmap_list);
}
#endif


/*
 * ALTREP Methods
 */

static SEXP mmap_Serialized_state(SEXP x)
{
    /* If serOK is FALSE then serialize as a regular typed vector. If
       serOK is true, then serialize information to allow the mmap to
       be reconstructed. The original file name is serialized; it will
       be expanded again when unserializing, in a context where the
       result may be different. */
    if (MMAP_SEROK(x))
	return MMAP_STATE(x);
    else
	return nullptr;
}

static SEXP mmap_file(SEXP, SEXPTYPE, Rboolean, Rboolean, Rboolean, Rboolean);

static SEXP mmap_Unserialize(SEXP class_, SEXP state)
{
    SEXP file = MMAP_STATE_FILE(state);
    SEXPTYPE type = SEXPTYPE(MMAP_STATE_TYPE(state));
    Rboolean ptrOK = Rboolean(MMAP_STATE_PTROK(state));
    Rboolean wrtOK = Rboolean(MMAP_STATE_WRTOK(state));
    Rboolean serOK = Rboolean(MMAP_STATE_SEROK(state));

    SEXP val = mmap_file(file, type, ptrOK, wrtOK, serOK, TRUE);
    if (val == nullptr) {
	/**** The attempt to memory map failed. Eventually it would be
	      good to have a mechanism to allow the user to try to
	      resolve this.  For now, return a length zero vector with
	      another warning. */
	Rf_warning("memory mapping failed; returning vector of length zero");
	return Rf_allocVector(type, 0);
    }
    return val;
}

Rboolean mmap_Inspect(SEXP x, int pre, int deep, int pvec,
		      void (*inspect_subtree)(SEXP, int, int, int))
{
    Rboolean ptrOK = Rboolean(MMAP_PTROK(x));
    Rboolean wrtOK = Rboolean(MMAP_WRTOK(x));
    Rboolean serOK = Rboolean(MMAP_SEROK(x));
    Rprintf(" mmaped %s", Rf_type2char(TYPEOF(x)));
    Rprintf(" [ptr=%d,wrt=%d,ser=%d]\n", ptrOK, wrtOK, serOK);
    return TRUE;
}


/*
 * ALTVEC Methods
 */

static R_xlen_t mmap_Length(SEXP x)
{
    return MMAP_LENGTH(x);
}

static void *mmap_Dataptr(SEXP x, Rboolean writeable)
{
    /* get addr first to get error if the object has been unmapped */
    void *addr = MMAP_ADDR(x);

    if (MMAP_PTROK(x))
	return addr;
    else
	Rf_error("cannot access data pointer for this mmaped vector");
}

static const void *mmap_Dataptr_or_null(SEXP x)
{
    return MMAP_PTROK(x) ? MMAP_ADDR(x) : nullptr;
}


/*
 * ALTINTEGER Methods
 */

static int mmap_integer_Elt(SEXP x, R_xlen_t i)
{
    int *p = static_cast<int *>(MMAP_ADDR(x));
    return p[i];
}

static
R_xlen_t mmap_integer_Get_region(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf)
{
    int *x = static_cast<int *>(MMAP_ADDR(sx));
    R_xlen_t size = XLENGTH(sx);
    R_xlen_t ncopy = size - i > n ? n : size - i;
    for (R_xlen_t k = 0; k < ncopy; k++)
	buf[k] = x[k + i];
    //memcpy(buf, x + i, ncopy * sizeof(int));
    return ncopy;
}


/*
 * ALTREAL Methods
 */

static double mmap_real_Elt(SEXP x, R_xlen_t i)
{
    double *p = static_cast<double *>(MMAP_ADDR(x));
    return p[i];
}

static
R_xlen_t mmap_real_Get_region(SEXP sx, R_xlen_t i, R_xlen_t n, double *buf)
{
    double *x = static_cast<double *>(MMAP_ADDR(sx));
    R_xlen_t size = XLENGTH(sx);
    R_xlen_t ncopy = size - i > n ? n : size - i;
    for (R_xlen_t k = 0; k < ncopy; k++)
	buf[k] = x[k + i];
    //memcpy(buf, x + i, ncopy * sizeof(double));
    return ncopy;
}


/*
 * Class Objects and Method Tables
 */

#ifdef SIMPLEMMAP
# define MMAPPKG "simplemmap"
#else
# define MMAPPKG "base"
#endif

static void InitMmapIntegerClass(DllInfo *dll)
{
    R_altrep_class_t cls =
	R_make_altinteger_class("mmap_integer", MMAPPKG, dll);
    mmap_integer_class = cls;

    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, mmap_Unserialize);
    R_set_altrep_Serialized_state_method(cls, mmap_Serialized_state);
    R_set_altrep_Inspect_method(cls, mmap_Inspect);
    R_set_altrep_Length_method(cls, mmap_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, mmap_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, mmap_Dataptr_or_null);

    /* override ALTINTEGER methods */
    R_set_altinteger_Elt_method(cls, mmap_integer_Elt);
    R_set_altinteger_Get_region_method(cls, mmap_integer_Get_region);
}

static void InitMmapRealClass(DllInfo *dll)
{
    R_altrep_class_t cls =
	R_make_altreal_class("mmap_real", MMAPPKG, dll);
    mmap_real_class = cls;

    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, mmap_Unserialize);
    R_set_altrep_Serialized_state_method(cls, mmap_Serialized_state);
    R_set_altrep_Inspect_method(cls, mmap_Inspect);
    R_set_altrep_Length_method(cls, mmap_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, mmap_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, mmap_Dataptr_or_null);

    /* override ALTREAL methods */
    R_set_altreal_Elt_method(cls, mmap_real_Elt);
    R_set_altreal_Get_region_method(cls, mmap_real_Get_region);
}


/*
 * Constructor
 */

#ifdef Win32
static void mmap_finalize(SEXP eptr)
{
    Rf_error("mmop objects not supported on Windows yet");
}

static SEXP mmap_file(SEXP file, SEXPTYPE type, Rboolean ptrOK, Rboolean wrtOK,
		      Rboolean serOK, Rboolean warn)
{
    Rf_error("mmop objects not supported on Windows yet");
}
#else
/* derived from the example in
  https://www.safaribooksonline.com/library/view/linux-system-programming/0596009585/ch04s03.html */

#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <sys/mman.h>

//#define DEBUG_PRINT(x) REprintf(x);
#define DEBUG_PRINT(x) do { } while (0)

static void mmap_finalize(SEXP eptr)
{
    DEBUG_PRINT("finalizing ... ");
    void *p = R_ExternalPtrAddr(eptr);
    size_t size = MMAP_STATE_SIZE(MMAP_EPTR_STATE(eptr));
    R_SetExternalPtrAddr(eptr, nullptr);

    if (p != nullptr) {
	munmap(p, size); /* don't check for errors */
	R_SetExternalPtrAddr(eptr, nullptr);
    }
    DEBUG_PRINT("done\n");
}

#define MMAP_FILE_WARNING_OR_ERROR(str, ...) do {	\
	if (warn) {					\
	    Rf_warning(str, __VA_ARGS__);			\
	    return nullptr;				\
	}						\
	else Rf_error(str, __VA_ARGS__);			\
    } while (0)

static SEXP mmap_file(SEXP file, SEXPTYPE type, Rboolean ptrOK, Rboolean wrtOK,
		      Rboolean serOK, Rboolean warn)
{
    const char *efn = R_ExpandFileName(Rf_translateChar(STRING_ELT(file, 0)));
    struct stat sb;

    /* Target not link */
    if (stat(efn, &sb) != 0)
	MMAP_FILE_WARNING_OR_ERROR("stat: %s", strerror(errno));

    if (! S_ISREG(sb.st_mode))
	MMAP_FILE_WARNING_OR_ERROR("%s is not a regular file", efn);

    int oflags = wrtOK ? O_RDWR : O_RDONLY;
    int fd = open(efn, oflags);
    if (fd == -1)
	MMAP_FILE_WARNING_OR_ERROR("open: %s", strerror(errno));

    int pflags = wrtOK ? PROT_READ | PROT_WRITE : PROT_READ;
    void *p = mmap(0, sb.st_size, pflags, MAP_SHARED, fd, 0);
    close(fd); /* don't care if this fails */
    if (p == MAP_FAILED)
	MMAP_FILE_WARNING_OR_ERROR("mmap: %s", strerror(errno));

    return make_mmap(p, file, sb.st_size, type, ptrOK, wrtOK, serOK);
}
#endif

static Rboolean asLogicalNA(SEXP x, Rboolean dflt)
{
    Rboolean val = Rboolean(Rf_asLogical(x));
    return val == R_NaLog ? dflt : val;
}

#ifdef SIMPLEMMAP
SEXP do_mmap_file(SEXP args)
{
    args = CDR(args);
#else
HIDDEN SEXP do_mmap_file(SEXP call, SEXP op, SEXP args, SEXP env)
{
#endif
    SEXP file = CAR(args);
    SEXP stype = CADR(args);
    SEXP sptrOK = CADDR(args);
    SEXP swrtOK = CADDDR(args);
    SEXP sserOK = CADDDR(CDR(args));

    SEXPTYPE type = REALSXP;
    if (stype != nullptr) {
	const char *typestr = R_CHAR(Rf_asChar(stype));
	if (streql(typestr, "double"))
	    type = REALSXP;
	else if (streql(typestr, "integer") || streql(typestr, "int"))
	    type = INTSXP;
	else
	    Rf_error("type '%s' is not supported", typestr);
    }    

    Rboolean ptrOK = (sptrOK == nullptr) ? TRUE : asLogicalNA(sptrOK, FALSE);
    Rboolean wrtOK = (swrtOK == nullptr) ? FALSE : asLogicalNA(swrtOK, FALSE);
    Rboolean serOK = (sserOK == nullptr) ? FALSE : asLogicalNA(sserOK, FALSE);

    if (TYPEOF(file) != STRSXP || LENGTH(file) != 1 || file == R_NaString)
	Rf_error("invalud 'file' argument");

    return mmap_file(file, type, ptrOK, wrtOK, serOK, FALSE);
}

#ifdef SIMPLEMMAP
static SEXP do_munmap_file(SEXP args)
{
    args = CDR(args);
#else
HIDDEN SEXP do_munmap_file(SEXP call, SEXP op, SEXP args, SEXP env)
{
#endif
    SEXP x = CAR(args);

    /**** would be useful to have R_mmap_class virtual class as parent here */
    if (! (R_altrep_inherits(x, mmap_integer_class) ||
	   R_altrep_inherits(x, mmap_real_class)))
	Rf_error("not a memory-mapped object");

    /* using the finalizer is a cheat to avoid yet another #ifdef Windows */
    SEXP eptr = MMAP_EPTR(x);
    errno = 0;
    rho::WeakRef::R_RunWeakRefFinalizer(R_ExternalPtrTag(eptr));
    if (errno)
	Rf_error("munmap: %s", strerror(errno));
    return nullptr;
}


/**
 ** Attribute and Meta Data Wrappers
 **/

/*
 * Wrapper Classes and Objects
 */

#define NMETA 2

static R_altrep_class_t wrap_integer_class;
static R_altrep_class_t wrap_real_class;
static R_altrep_class_t wrap_string_class;

/* Wrapper objects are ALTREP objects designed to hold the attributes
   of a potentially large object and/or meta data for the object. */

#define WRAPPER_WRAPPED(x) R_altrep_data1(x)
#define WRAPPER_SET_WRAPPED(x, v) R_set_altrep_data1(x, v)
#define WRAPPER_METADATA(x) R_altrep_data2(x)
#define WRAPPER_SET_METADATA(x, v) R_set_altrep_data2(x, v)

#define WRAPPER_SORTED(x) INTEGER(WRAPPER_METADATA(x))[0]
#define WRAPPER_NO_NA(x) INTEGER(WRAPPER_METADATA(x))[1]


/*
 * ALTREP Methods
 */

static SEXP wrapper_Serialized_state(SEXP x)
{
    return CONS(WRAPPER_WRAPPED(x), WRAPPER_METADATA(x));
}

static SEXP make_wrapper(SEXP, SEXP);

static SEXP wrapper_Unserialize(SEXP class_, SEXP state)
{
    return make_wrapper(CAR(state), CDR(state));
}

static SEXP wrapper_Duplicate(SEXP x, Rboolean deep)
{
    SEXP data = WRAPPER_WRAPPED(x);

    /* For a deep copy, duplicate the data. */
    /* For a shallow copy, mark as immutable in the NAMED world; with
       reference counting the reference count will be incremented when
       the data is installed in the new wrapper object. */
    if (deep)
	data = Rf_duplicate(data);
#ifndef SWITCH_TO_REFCNT
    else
	/* not needed with reference counting */
	MARK_NOT_MUTABLE(data);
#endif
    PROTECT(data);

    /* always duplicate the meta data */
    SEXP meta = PROTECT(Rf_duplicate(WRAPPER_METADATA(x)));

    SEXP ans = make_wrapper(data, meta);

    UNPROTECT(2); /* data, meta */
    return ans;
}

Rboolean wrapper_Inspect(SEXP x, int pre, int deep, int pvec,
			 void (*inspect_subtree)(SEXP, int, int, int))
{
    Rboolean srt = Rboolean(WRAPPER_SORTED(x));
    Rboolean no_na = Rboolean(WRAPPER_NO_NA(x));
    Rprintf(" wrapper [srt=%d,no_na=%d]\n", srt, no_na);
    inspect_subtree(WRAPPER_WRAPPED(x), pre, deep, pvec);
    return TRUE;
}

static R_xlen_t wrapper_Length(SEXP x)
{
    return XLENGTH(WRAPPER_WRAPPED(x));
}


/*
 * ALTVEC Methods
 */

static void clear_meta_data(SEXP x)
{
    SEXP meta = WRAPPER_METADATA(x);
    INTEGER(meta)[0] = UNKNOWN_SORTEDNESS;
    for (int i = 1; i < NMETA; i++)
	INTEGER(meta)[i] = 0;
}

static void *wrapper_Dataptr(SEXP x, Rboolean writeable)
{
    SEXP data = WRAPPER_WRAPPED(x);

    /* If the data might be shared and a writeable pointer is
       requested, then the data needs to be duplicated now. */
    if (writeable && MAYBE_SHARED(data)) {
	PROTECT(x);
	WRAPPER_SET_WRAPPED(x, Rf_shallow_duplicate(data));
	UNPROTECT(1);
    }

    if (writeable) {
	/* If a writeable pointer is requested then the meta-data needs
	   to be cleared as it may no longer be valid after a write. */
	clear_meta_data(x);
	return DATAPTR(WRAPPER_WRAPPED(x));
    }
    else
	/**** avoid the cast by having separate methods */
	return (void *) DATAPTR_RO(WRAPPER_WRAPPED(x));
}

static const void *wrapper_Dataptr_or_null(SEXP x)
{
    return DATAPTR_OR_NULL(WRAPPER_WRAPPED(x));
}


/*
 * ALTINTEGER Methods
 */

static int wrapper_integer_Elt(SEXP x, R_xlen_t i)
{
    return INTEGER_ELT(WRAPPER_WRAPPED(x), i);
}

static
R_xlen_t wrapper_integer_Get_region(SEXP x, R_xlen_t i, R_xlen_t n, int *buf)
{
    return INTEGER_GET_REGION(WRAPPER_WRAPPED(x), i, n, buf);
}

static int wrapper_integer_Is_sorted(SEXP x)
{
    if (WRAPPER_SORTED(x) != UNKNOWN_SORTEDNESS)
	return WRAPPER_SORTED(x);
    else
	/* If the  meta data bit is not set, defer to the wrapped object. */
	return INTEGER_IS_SORTED(WRAPPER_WRAPPED(x));
}

static int wrapper_integer_no_NA(SEXP x)
{
    if (WRAPPER_NO_NA(x))
	return TRUE;
    else
	/* If the  meta data bit is not set, defer to the wrapped object. */
	return INTEGER_NO_NA(WRAPPER_WRAPPED(x));
}


/*
 * ALTREAL Methods
 */

static double wrapper_real_Elt(SEXP x, R_xlen_t i)
{
    return REAL_ELT(WRAPPER_WRAPPED(x), i);
}

static
R_xlen_t wrapper_real_Get_region(SEXP x, R_xlen_t i, R_xlen_t n, double *buf)
{
    return REAL_GET_REGION(WRAPPER_WRAPPED(x), i, n, buf);
}

static int wrapper_real_Is_sorted(SEXP x)
{
    if (WRAPPER_SORTED(x) != UNKNOWN_SORTEDNESS)
	return WRAPPER_SORTED(x);
    else
	/* If the  meta data bit is not set, defer to the wrapped object. */
	return REAL_IS_SORTED(WRAPPER_WRAPPED(x));
}

static int wrapper_real_no_NA(SEXP x)
{
    if (WRAPPER_NO_NA(x))
	return TRUE;
    else
	/* If the  meta data bit is not set, defer to the wrapped object. */
	return REAL_NO_NA(WRAPPER_WRAPPED(x));
}


/*
 * ALTSTRING Methods
 */

static SEXP wrapper_string_Elt(SEXP x, R_xlen_t i)
{
    return STRING_ELT(WRAPPER_WRAPPED(x), i);
}

static int wrapper_string_Is_sorted(SEXP x)
{
    if (WRAPPER_SORTED(x) != UNKNOWN_SORTEDNESS)
	return WRAPPER_SORTED(x);
    else
	/* If the  meta data bit is not set, defer to the wrapped object. */
	return STRING_IS_SORTED(WRAPPER_WRAPPED(x));
}

static int wrapper_string_no_NA(SEXP x)
{
    if (WRAPPER_NO_NA(x))
	return TRUE;
    else
	/* If the  meta data bit is not set, defer to the wrapped object. */
	return STRING_NO_NA(WRAPPER_WRAPPED(x));
}


/*
 * Class Objects and Method Tables
 */

#define WRAPPKG "base"

static void InitWrapIntegerClass(DllInfo *dll)
{
    R_altrep_class_t cls =
	R_make_altinteger_class("wrap_integer", WRAPPKG, dll);
    wrap_integer_class = cls;

    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, wrapper_Unserialize);
    R_set_altrep_Serialized_state_method(cls, wrapper_Serialized_state);
    R_set_altrep_Duplicate_method(cls, wrapper_Duplicate);
    R_set_altrep_Inspect_method(cls, wrapper_Inspect);
    R_set_altrep_Length_method(cls, wrapper_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, wrapper_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, wrapper_Dataptr_or_null);

    /* override ALTINTEGER methods */
    R_set_altinteger_Elt_method(cls, wrapper_integer_Elt);
    R_set_altinteger_Get_region_method(cls, wrapper_integer_Get_region);
    R_set_altinteger_Is_sorted_method(cls, wrapper_integer_Is_sorted);
    R_set_altinteger_No_NA_method(cls, wrapper_integer_no_NA);
}

static void InitWrapRealClass(DllInfo *dll)
{
    R_altrep_class_t cls =
	R_make_altreal_class("wrap_real", WRAPPKG, dll);
    wrap_real_class = cls;

    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, wrapper_Unserialize);
    R_set_altrep_Serialized_state_method(cls, wrapper_Serialized_state);
    R_set_altrep_Duplicate_method(cls, wrapper_Duplicate);
    R_set_altrep_Inspect_method(cls, wrapper_Inspect);
    R_set_altrep_Length_method(cls, wrapper_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, wrapper_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, wrapper_Dataptr_or_null);

    /* override ALTREAL methods */
    R_set_altreal_Elt_method(cls, wrapper_real_Elt);
    R_set_altreal_Get_region_method(cls, wrapper_real_Get_region);
    R_set_altreal_Is_sorted_method(cls, wrapper_real_Is_sorted);
    R_set_altreal_No_NA_method(cls, wrapper_real_no_NA);
}

static void InitWrapStringClass(DllInfo *dll)
{
    R_altrep_class_t cls =
	R_make_altstring_class("wrap_string", WRAPPKG, dll);
    wrap_string_class = cls;

    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, wrapper_Unserialize);
    R_set_altrep_Serialized_state_method(cls, wrapper_Serialized_state);
    R_set_altrep_Duplicate_method(cls, wrapper_Duplicate);
    R_set_altrep_Inspect_method(cls, wrapper_Inspect);
    R_set_altrep_Length_method(cls, wrapper_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, wrapper_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, wrapper_Dataptr_or_null);

    /* override ALTSTRING methods */
    R_set_altstring_Elt_method(cls, wrapper_string_Elt);
    R_set_altstring_Is_sorted_method(cls, wrapper_string_Is_sorted);
    R_set_altstring_No_NA_method(cls, wrapper_string_no_NA);
}


/*
 * Constructor
 */

static SEXP make_wrapper(SEXP x, SEXP meta)
{
    /* If x is itself a wrapper it might be a good idea to fuse */
    R_altrep_class_t cls;
    switch(TYPEOF(x)) {
    case INTSXP: cls = wrap_integer_class; break;
    case REALSXP: cls = wrap_real_class; break;
    case STRSXP: cls = wrap_string_class; break;
    default: Rf_error("unsupported type");
    }

    SEXP ans = R_new_altrep(cls, x, meta);

#ifndef SWITCH_TO_REFCNT
    if (MAYBE_REFERENCED(x))
	/* make sure no mutation can happen through another reference */
	MARK_NOT_MUTABLE(x);
#endif

    return ans;
}

HIDDEN SEXP do_wrap_meta(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x = CAR(args);
    switch(TYPEOF(x)) {
    case INTSXP:
    case REALSXP:
    case STRSXP: break;
    default: Rf_error("only INTSXP, REALSXP, STRSXP vectors suppoted for now");
    }

    if (ATTRIB(x) != nullptr)
	/* For objects without references we could move the attributes
	   to the wrapper. For objects with references the attributes
	   would have to be shallow duplicated at least. The object/S4
	   bits would need to be moved as well.	*/
	/* For now, just return the original object. */
	return x;

    int srt = Rf_asInteger(CADR(args));
    if (!KNOWN_SORTED(srt) && srt != KNOWN_UNSORTED &&
	srt != UNKNOWN_SORTEDNESS)
	Rf_error("srt must be -2, -1, 0, or +1, +2, or NA");

    int no_na = Rf_asInteger(CADDR(args));
    if (no_na < 0 || no_na > 1)
	Rf_error("no_na must be 0 or +1");

    SEXP meta = Rf_allocVector(INTSXP, NMETA);
    INTEGER(meta)[0] = srt;
    INTEGER(meta)[1] = no_na;

    return make_wrapper(x, meta);
}


/**
 ** Initialize ALTREP Classes
 **/

HIDDEN void R_init_altrep()
{
    InitCompactIntegerClass();
    InitCompactRealClass();
    InitDefferredStringClass();
    InitMmapIntegerClass(nullptr);
    InitMmapRealClass(nullptr);
    InitWrapIntegerClass(nullptr);
    InitWrapRealClass(nullptr);
    InitWrapStringClass(nullptr);
}
