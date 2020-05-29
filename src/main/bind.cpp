/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997--2017  The R Core Team
 *  Copyright (C) 2002--2017  The R Foundation
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

/* Code to handle list / vector switch */

#define R_NO_REMAP

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#include <Localization.h>
#include <Internal.h>
#include <R_ext/PrtUtil.h> // for IndexWidth
#include <R_ext/Itermacros.h>

#include "duplicate.h"
#include "RBufferUtils.h"
#include "rho/Closure.hpp"
#include "rho/ExpressionVector.hpp"
#include "rho/GCStackRoot.hpp"

using namespace std;
using namespace rho;

static R_StringBuffer cbuff = {nullptr, 0, MAXELTSIZE};

static RObject* cbind(RObject*, const ArgList& args, SEXPTYPE, RObject*, int);
static RObject* rbind(RObject*, const ArgList& args, SEXPTYPE, RObject*, int);
static RObject* do_c_dflt(Expression* call, BuiltInFunction* op, ArgList&& args, RObject* env);

/* The following code establishes the return type for the */
/* functions  unlist, c, cbind, and rbind and also determines */
/* whether the returned object is to have a names attribute. */

struct BindData {
 int  ans_flags;
 SEXP ans_ptr;
 R_xlen_t ans_length;
 SEXP ans_names;
 R_xlen_t  ans_nnames;
/* int  deparse_level; Initialize to 1. */
};

static bool HasNames(RObject* x)
{
    if(Rf_isVector(x)) {
	if (!Rf_isNull(Rf_getAttrib(x, Symbols::NamesSymbol)))
	    return true;
    }
    else if(Rf_isList(x)) {
	while (!Rf_isNull(x)) {
	    if (!Rf_isNull(TAG(x))) return 1;
	    x = CDR(x);
	}
    }
    return false;
}

static void
AnswerType(RObject* x, bool recurse, bool usenames, struct BindData *data, Expression* call)
{
    switch (TYPEOF(x)) {
    case NILSXP:
	break;
    case RAWSXP:
	data->ans_flags |= 1;
	data->ans_length += XLENGTH(x);
	break;
    case LGLSXP:
	data->ans_flags |= 2;
	data->ans_length += XLENGTH(x);
	break;
    case INTSXP:
	data->ans_flags |= 16;
	data->ans_length += XLENGTH(x);
	break;
    case REALSXP:
	data->ans_flags |= 32;
	data->ans_length += XLENGTH(x);
	break;
    case CPLXSXP:
	data->ans_flags |= 64;
	data->ans_length += XLENGTH(x);
	break;
    case STRSXP:
	data->ans_flags |= 128;
	data->ans_length += XLENGTH(x);
	break;

#ifdef DO_C_Symbol
/* new case : */
    case SYMSXP:
    case LANGSXP:
	data->ans_flags |= 512; /* as.expression() implicitly */
	data->ans_length += 1;
	break;
#endif
    case VECSXP:
	if (recurse) {
	    R_xlen_t i, n = Rf_xlength(x);
	    if (usenames && !data->ans_nnames &&
		!Rf_isNull(Rf_getAttrib(x, Symbols::NamesSymbol)))
		data->ans_nnames = 1;
	    for (i = 0; i < n; i++) {
		if (usenames && !data->ans_nnames)
		    data->ans_nnames = HasNames(VECTOR_ELT(x, i));
		AnswerType(VECTOR_ELT(x, i), recurse, usenames, data, call);
	    }
	}
	else {
	    data->ans_flags |= 256;
	    data->ans_length += Rf_xlength(x);
	}
	break;
    case EXPRSXP:
	if (recurse) {
	    R_xlen_t i, n = Rf_xlength(x);
	    if (usenames && !data->ans_nnames &&
		!Rf_isNull(Rf_getAttrib(x, Symbols::NamesSymbol)))
		data->ans_nnames = 1;
	    for (i = 0; i < n; i++) {
		if (usenames && !data->ans_nnames)
		    data->ans_nnames = HasNames(XVECTOR_ELT(x, i));
		AnswerType(XVECTOR_ELT(x, i), recurse, usenames, data, call);
	    }
	}
	else {
	    data->ans_flags |= 512;
	    data->ans_length += Rf_xlength(x);
	}
	break;
    case LISTSXP:
	if (recurse) {
	    while (x != nullptr) {
		if (usenames && !data->ans_nnames) {
		    if (!Rf_isNull(TAG(x))) data->ans_nnames = 1;
		    else data->ans_nnames = HasNames(CAR(x));
		}
		AnswerType(CAR(x), recurse, usenames, data, call);
		x = CDR(x);
	    }
	}
	else {
	    data->ans_flags |= 256;
	    data->ans_length += Rf_length(x);
	}
	break;
    default:
	data->ans_flags |= 256;
	data->ans_length += 1;
	break;
    }

    /* check for overflow in ans_length. Objects are added one at a
       time for each call to AnswerType so it is safe to check here.
       Since sizes are signed, positive numbers, the overflow will
       manifest itself as a negative result (both numbers will be
       31-bit so we cannot overflow across the 32-bit boundary). If
       our assumption (all lengths are signed) is violated, this won't
       work so check when switching length types! */

#ifndef LONG_VECTOR_SUPPORT
    if (data->ans_length < 0)
	Rf_errorcall(call, _("resulting vector exceeds vector length limit in '%s'"), "AnswerType");
#endif
}


/* The following functions are used to coerce arguments to the
 * appropriate type for inclusion in the returned value. */

namespace {
    inline void LIST_ASSIGN(struct BindData* data, SEXP x)
    {
	if (ExpressionVector* ev
	    = dynamic_cast<ExpressionVector*>(data->ans_ptr))
	    (*ev)[data->ans_length] = x;
	else
	    SET_VECTOR_ELT(data->ans_ptr, data->ans_length, x);
	data->ans_length++;
    }
}

static void
ListAnswer(RObject* x, int recurse, struct BindData *data, Expression* call)
{
    R_xlen_t i;

    switch(TYPEOF(x)) {
    case NILSXP:
	break;
    case LGLSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    LIST_ASSIGN(data, Rf_ScalarLogical(LOGICAL(x)[i]));
	break;
    case RAWSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    LIST_ASSIGN(data, Rf_ScalarRaw(RAW(x)[i]));
	break;
    case INTSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    LIST_ASSIGN(data, Rf_ScalarInteger(INTEGER(x)[i]));
	break;
    case REALSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    LIST_ASSIGN(data, Rf_ScalarReal(REAL(x)[i]));
	break;
    case CPLXSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    LIST_ASSIGN(data, Rf_ScalarComplex(COMPLEX(x)[i]));
	break;
    case STRSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    LIST_ASSIGN(data, Rf_ScalarString(STRING_ELT(x, i)));
	break;
    case VECSXP:
	if (recurse) {
	    for (i = 0; i < XLENGTH(x); i++)
		ListAnswer(VECTOR_ELT(x, i), recurse, data, call);
	}
	else {
	    for (i = 0; i < XLENGTH(x); i++)
		LIST_ASSIGN(data, Rf_lazy_duplicate(VECTOR_ELT(x, i)));
	}
	break;
    case EXPRSXP:
	if (recurse) {
	    for (i = 0; i < XLENGTH(x); i++)
		ListAnswer(XVECTOR_ELT(x, i), recurse, data, call);
	}
	else {
	    for (i = 0; i < XLENGTH(x); i++)
		LIST_ASSIGN(data, Rf_duplicate(XVECTOR_ELT(x, i)));
	}
	break;
    case LISTSXP:
	if (recurse) {
	    while (x != nullptr) {
		ListAnswer(CAR(x), recurse, data, call);
		x = CDR(x);
	    }
	}
	else
	    while (x != nullptr) {
		LIST_ASSIGN(data, Rf_lazy_duplicate(CAR(x)));
		x = CDR(x);
	    }
	break;
    default:
	LIST_ASSIGN(data, Rf_lazy_duplicate(x));
	break;
    }
}

static void
StringAnswer(RObject* x, struct BindData *data, Expression* call)
{
    R_xlen_t i;
    switch(TYPEOF(x)) {
    case NILSXP:
	break;
    case LISTSXP:
	while (x != nullptr) {
	    StringAnswer(CAR(x), data, call);
	    x = CDR(x);
	}
	break;
    case EXPRSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    StringAnswer(XVECTOR_ELT(x, i), data, call);
	break;
    case VECSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    StringAnswer(VECTOR_ELT(x, i), data, call);
	break;
    default:
	PROTECT(x = Rf_coerceVector(x, STRSXP));
	for (i = 0; i < XLENGTH(x); i++)
	    SET_STRING_ELT(data->ans_ptr, data->ans_length++, STRING_ELT(x, i));
	UNPROTECT(1);
	break;
    }
}

static void
LogicalAnswer(RObject* x, struct BindData *data, Expression* call)
{
    R_xlen_t i;
    switch(TYPEOF(x)) {
    case NILSXP:
	break;
    case LISTSXP:
	while (x != nullptr) {
	    LogicalAnswer(CAR(x), data, call);
	    x = CDR(x);
	}
	break;
    case EXPRSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    LogicalAnswer(XVECTOR_ELT(x, i), data, call);
	break;
    case VECSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    LogicalAnswer(VECTOR_ELT(x, i), data, call);
	break;
    case LGLSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    LOGICAL(data->ans_ptr)[data->ans_length++] = LOGICAL(x)[i];
	break;
    case INTSXP:
	for (i = 0; i < XLENGTH(x); i++) {
	    int v = INTEGER(x)[i];
	    LOGICAL(data->ans_ptr)[data->ans_length++] = (v == NA_INTEGER) ? NA_LOGICAL : ( v != 0 );
	}
	break;
    case RAWSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    LOGICAL(data->ans_ptr)[data->ans_length++] = (int)RAW(x)[i] != 0;
	break;
    default:
	Rf_errorcall(call, _("type '%s' is unimplemented in '%s'"),
		  Rf_type2char(TYPEOF(x)), "LogicalAnswer");
    }
}

static void
IntegerAnswer(RObject* x, struct BindData *data, Expression* call)
{
    R_xlen_t i;
    switch(TYPEOF(x)) {
    case NILSXP:
	break;
    case LISTSXP:
	while (x != nullptr) {
	    IntegerAnswer(CAR(x), data, call);
	    x = CDR(x);
	}
	break;
    case EXPRSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    IntegerAnswer(XVECTOR_ELT(x, i), data, call);
	break;
    case VECSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    IntegerAnswer(VECTOR_ELT(x, i), data, call);
	break;
    case LGLSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    INTEGER(data->ans_ptr)[data->ans_length++] = LOGICAL(x)[i];
	break;
    case INTSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    INTEGER(data->ans_ptr)[data->ans_length++] = INTEGER(x)[i];
	break;
    case RAWSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    INTEGER(data->ans_ptr)[data->ans_length++] = int(RAW(x)[i]);
	break;
    default:
	Rf_errorcall(call, _("type '%s' is unimplemented in '%s'"),
		  Rf_type2char(TYPEOF(x)), "IntegerAnswer");
    }
}

static void
RealAnswer(RObject* x, struct BindData *data, Expression* call)
{
    R_xlen_t i;
    int xi;
    switch(TYPEOF(x)) {
    case NILSXP:
	break;
    case LISTSXP:
	while (x != nullptr) {
	    RealAnswer(CAR(x), data, call);
	    x = CDR(x);
	}
	break;
    case VECSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    RealAnswer(VECTOR_ELT(x, i), data, call);
	break;
    case EXPRSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    RealAnswer(XVECTOR_ELT(x, i), data, call);
	break;
    case REALSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    REAL(data->ans_ptr)[data->ans_length++] = REAL(x)[i];
	break;
    case LGLSXP:
	for (i = 0; i < XLENGTH(x); i++) {
	    xi = LOGICAL(x)[i];
	    if (xi == NA_LOGICAL)
		REAL(data->ans_ptr)[data->ans_length++] = NA_REAL;
	    else REAL(data->ans_ptr)[data->ans_length++] = xi;
	}
	break;
    case INTSXP:
	for (i = 0; i < XLENGTH(x); i++) {
	    xi = INTEGER(x)[i];
	    if (xi == NA_INTEGER)
		REAL(data->ans_ptr)[data->ans_length++] = NA_REAL;
	    else REAL(data->ans_ptr)[data->ans_length++] = xi;
	}
	break;
    case RAWSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    REAL(data->ans_ptr)[data->ans_length++] = int(RAW(x)[i]);
	break;
    default:
	Rf_errorcall(call, _("type '%s' is unimplemented in '%s'"),
		  Rf_type2char(TYPEOF(x)), "RealAnswer");
    }
}

static void
ComplexAnswer(RObject* x, struct BindData *data, Expression* call)
{
    R_xlen_t i;
    int xi;
    switch(TYPEOF(x)) {
    case NILSXP:
	break;
    case LISTSXP:
	while (x != nullptr) {
	    ComplexAnswer(CAR(x), data, call);
	    x = CDR(x);
	}
	break;
    case EXPRSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    ComplexAnswer(XVECTOR_ELT(x, i), data, call);
	break;
    case VECSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    ComplexAnswer(VECTOR_ELT(x, i), data, call);
	break;
    case REALSXP:
	for (i = 0; i < XLENGTH(x); i++) {
	    COMPLEX(data->ans_ptr)[data->ans_length].r = REAL(x)[i];
	    COMPLEX(data->ans_ptr)[data->ans_length].i = 0.0;
	    data->ans_length++;
	}
	break;
    case CPLXSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    COMPLEX(data->ans_ptr)[data->ans_length++] = COMPLEX(x)[i];
	break;
    case LGLSXP:
	for (i = 0; i < XLENGTH(x); i++) {
	    xi = LOGICAL(x)[i];
	    if (xi == NA_LOGICAL) {
		COMPLEX(data->ans_ptr)[data->ans_length].r = NA_REAL;
		COMPLEX(data->ans_ptr)[data->ans_length].i = NA_REAL;
	    }
	    else {
		COMPLEX(data->ans_ptr)[data->ans_length].r = xi;
		COMPLEX(data->ans_ptr)[data->ans_length].i = 0.0;
	    }
	    data->ans_length++;
	}
	break;
    case INTSXP:
	for (i = 0; i < XLENGTH(x); i++) {
	    xi = INTEGER(x)[i];
	    if (xi == NA_INTEGER) {
		COMPLEX(data->ans_ptr)[data->ans_length].r = NA_REAL;
		COMPLEX(data->ans_ptr)[data->ans_length].i = NA_REAL;
	    }
	    else {
		COMPLEX(data->ans_ptr)[data->ans_length].r = xi;
		COMPLEX(data->ans_ptr)[data->ans_length].i = 0.0;
	    }
	    data->ans_length++;
	}
	break;

    case RAWSXP:
	for (i = 0; i < XLENGTH(x); i++) {
	    COMPLEX(data->ans_ptr)[data->ans_length].r = int(RAW(x)[i]);
	    COMPLEX(data->ans_ptr)[data->ans_length].i = 0.0;
	    data->ans_length++;
	}
	break;

    default:
	Rf_errorcall(call, _("type '%s' is unimplemented in '%s'"),
		  Rf_type2char(TYPEOF(x)), "ComplexAnswer");
    }
}

static void
RawAnswer(RObject* x, struct BindData *data, Expression* call)
{
    R_xlen_t i;
    switch(TYPEOF(x)) {
    case NILSXP:
	break;
    case LISTSXP:
	while (x != nullptr) {
	    RawAnswer(CAR(x), data, call);
	    x = CDR(x);
	}
	break;
    case EXPRSXP:
    case VECSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    RawAnswer(VECTOR_ELT(x, i), data, call);
	break;
    case RAWSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    RAW(data->ans_ptr)[data->ans_length++] = RAW(x)[i];
	break;
    default:
	Rf_errorcall(call, _("type '%s' is unimplemented in '%s'"),
		  Rf_type2char(TYPEOF(x)), "RawAnswer");
    }
}

static RObject* NewBase(RObject* base, RObject* tag)
{
    RObject* ans;
    char *cbuf;
    base = Rf_EnsureString(base);
    tag = Rf_EnsureString(tag);
    if (*R_CHAR(base) && *R_CHAR(tag)) { /* test of length */
	const void *vmax = vmaxget();
	const char *sb = Rf_translateCharUTF8(base), *st = Rf_translateCharUTF8(tag);
	cbuf = static_cast<char*>(R_AllocStringBuffer(strlen(st) + strlen(sb) + 1, &cbuff));
	sprintf(cbuf, "%s.%s", sb, st);
	/* This isn't strictly correct as we do not know that all the
	   components of the name were correctly translated. */
	ans = Rf_mkCharCE(cbuf, CE_UTF8);
	vmaxset(vmax);
    }
    else if (*R_CHAR(tag)) {
	ans = tag;
    }
    else if (*R_CHAR(base)) {
	ans = base;
    }
    else ans = R_BlankString;
    return ans;
}

static RObject* NewName(RObject* base, RObject* tag, R_xlen_t seqno, int count)
{
/* Construct a new Name/Tag, using
 *	base.tag
 *	base
 *	base<seqno>	or
 *	tag
 *
 */
    SEXP ans;
    base = Rf_EnsureString(base);
    tag = Rf_EnsureString(tag);
    if (*R_CHAR(base)) {
	if (*R_CHAR(tag)) {
	    const void *vmax = vmaxget();
	    const char
		*sb = Rf_translateCharUTF8(base),
		*st = Rf_translateCharUTF8(tag);
	    char *cbuf = static_cast<char*>(R_AllocStringBuffer(strlen(sb) + strlen(st) + 1, &cbuff));
	    sprintf(cbuf, "%s.%s", sb, st);
	    ans = Rf_mkCharCE(cbuf, CE_UTF8);
	    vmaxset(vmax);
	}
	else if (count == 1)
	    ans = base;
	else {
	    const void *vmax = vmaxget();
	    const char *sb = Rf_translateCharUTF8(base);
	    char *cbuf;
	    cbuf = static_cast<char*>(R_AllocStringBuffer(strlen(sb) + size_t(Rf_IndexWidth(seqno)),
				       &cbuff));
#ifdef LONG_VECTOR_SUPPORT
	    if (seqno > INT_MAX)
		sprintf(cbuf, "%s%.0f", sb, (double) seqno);
	    else
#endif
		sprintf(cbuf, "%s%d", sb, (int) seqno);
	    ans = Rf_mkCharCE(cbuf, CE_UTF8);
	    vmaxset(vmax);
	}
    }
    else if (*R_CHAR(tag)) {
	ans = tag;
    } else
	ans = R_BlankString;
    return ans;
}

/* also used in coerce.cpp */
HIDDEN SEXP Rf_ItemName(SEXP names, R_xlen_t i)
{
  /* return  names[i]  if it is a character (>= 1 char), or NULL otherwise */
    if (names != nullptr &&
	STRING_ELT(names, i) != nullptr &&
	R_CHAR(STRING_ELT(names, i))[0] != '\0') /* length test */
	return STRING_ELT(names, i);
    else
	return nullptr;
}

/* NewExtractNames(v, base, tag, recurse):  For c() and	 unlist().
 * On entry, "base" is the naming component we have acquired by
 * recursing down from above.
 *	If we have a list and we are recursing, we append a new tag component
 * to the base tag (either by using the list tags, or their offsets),
 * and then we do the recursion.
 *	If we have a vector, we just create the tags for each element. */

struct NameData {
 int count;
 R_xlen_t seqno;
 // int firstpos;
};


// count names in (branch) v, recursively if(recurse) :
static void namesCount(RObject* v, const int recurse, struct NameData *nameData)
{
    R_xlen_t i, n = Rf_xlength(v);
    SEXP names = PROTECT(Rf_getAttrib(v, Symbols::NamesSymbol)), namei;

    /* The  "<= 1"  in every for() loop, i.e., for all "vector" cases,
       makes this much faster for large vector 'v'
       _and_ prevents ("almost surely") overflow of nameData->count.
       -->  PR#17284 and PR#17292, with thanks to Suharto Anggono.
    */
    switch(TYPEOF(v)) {
    case NILSXP:
	break;
    case LISTSXP:
	if (recurse) {
	    for (i = 0; i < n && nameData->count <= 1; i++) {
		PROTECT(namei = Rf_ItemName(names, i));
		if (namei == nullptr)
		    namesCount(CAR(v), recurse, nameData);
		v = CDR(v);
		UNPROTECT(1); /*namei*/
	    }
	    break;
	} /* else fall through */
    case VECSXP:
    case EXPRSXP:
	if (recurse) {
	    for (i = 0; i < n && nameData->count <= 1; i++) {
		namei = Rf_ItemName(names, i);
		if (namei == nullptr)
		    namesCount(VECTOR_ELT(v, i), recurse, nameData);
	    }
	    break;
	} /* else fall through */
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case RAWSXP:
	for (i = 0; i < n && nameData->count <= 1; i++)
	    nameData->count++;
	break;
    default:
	nameData->count++;
    }
    UNPROTECT(1); /*names*/
}

static void NewExtractNames(RObject* v, RObject* base, const RObject* tag, int recurse,
			     struct BindData *data, struct NameData *nameData)
{
    SEXP names, namei;
    R_xlen_t i, n;
    int savecount=0;
    R_xlen_t saveseqno;

    /* If we have a new tag, we reset the index
     * sequence and create the new basename string : */
    if (tag != nullptr) {
        PROTECT(base = NewBase(base, const_cast<SEXP>(tag)));
	saveseqno = nameData->seqno;
	savecount = nameData->count;
	nameData->count = 0;
	namesCount(v, recurse, nameData);
	nameData->seqno = 0;
    }
    else saveseqno = 0;

    n = Rf_xlength(v);
    PROTECT(names = Rf_getAttrib(v, Symbols::NamesSymbol));

    switch(TYPEOF(v)) {
    case NILSXP:
	break;
    case LISTSXP:
	for (i = 0; i < n; i++) {
	    PROTECT(namei = Rf_ItemName(names, i));
	    if (recurse) {
		NewExtractNames(CAR(v), base, namei, recurse, data, nameData);
	    }
	    else {
		namei = NewName(base, namei, ++(nameData->seqno), nameData->count);
		SET_STRING_ELT(data->ans_names, (data->ans_nnames)++, namei);
	    }
	    v = CDR(v);
	    UNPROTECT(1); /*namei*/
	}
	break;
    case VECSXP:
	for (i = 0; i < n; i++) {
	    namei = Rf_ItemName(names, i);
	    if (recurse) {
		NewExtractNames(VECTOR_ELT(v, i), base, namei, recurse, data, nameData);
	    }
	    else {
		namei = NewName(base, namei, ++(nameData->seqno), nameData->count);
		SET_STRING_ELT(data->ans_names, (data->ans_nnames)++, namei);
	    }
	}
	break;
    case EXPRSXP:
	for (i = 0; i < n; i++) {
	    namei = Rf_ItemName(names, i);
	    if (recurse) {
		NewExtractNames(XVECTOR_ELT(v, i), base, namei, recurse, data, nameData);
	    }
	    else {
		namei = NewName(base, namei, ++(nameData->seqno), nameData->count);
		SET_STRING_ELT(data->ans_names, (data->ans_nnames)++, namei);
	    }
	}
	break;
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case RAWSXP:
	for (i = 0; i < n; i++) {
	    namei = Rf_ItemName(names, i);
	    namei = NewName(base, namei, ++(nameData->seqno), nameData->count);
	    SET_STRING_ELT(data->ans_names, (data->ans_nnames)++, namei);
	}
	break;
    default:
	namei = NewName(base, nullptr, ++(nameData->seqno), nameData->count);
	SET_STRING_ELT(data->ans_names, (data->ans_nnames)++, namei);
    }
    if (tag != nullptr) {
	nameData->count = savecount;
	UNPROTECT(1);
    }
    UNPROTECT(1); /*names*/
    nameData->seqno = nameData->seqno + saveseqno;
}

/* Code to extract the optional arguments to c().  We do it this */
/* way, rather than having an interpreted front-end do the job, */
/* because we want to avoid duplication at the top level. */
/* FIXME : is there another possibility? */
static RObject* c_Extract_opt(RObject* ans, Rboolean *recurse, Rboolean *usenames,
			  Expression* call)
{
    SEXP a, n, last = nullptr, next = nullptr;
    int v, n_recurse = 0, n_usenames = 0;

    for (a = ans; a != nullptr; a = next) {
	n = TAG(a);
	next = CDR(a);
	if (n != nullptr && Rf_pmatch(Symbols::RecursiveSymbol, n, TRUE)) {
	    if (n_recurse++ == 1)
		Rf_errorcall(call, _("repeated formal argument 'recursive'"));
	    if ((v = Rf_asLogical(CAR(a))) != NA_INTEGER) {
		*recurse = Rboolean(v);
	    }
	    if (last == nullptr)
		ans = next;
	    else
		SETCDR(last, next);
	}
	else if (n != nullptr && Rf_pmatch(Symbols::UseNamesSymbol, n, TRUE)) {
	    if (n_usenames++ == 1)
		Rf_errorcall(call, _("repeated formal argument 'use.names'"));
	    if ((v = Rf_asLogical(CAR(a))) != NA_INTEGER) {
		*usenames = Rboolean(v);
	    }
	    if (last == nullptr)
		ans = next;
	    else
		SETCDR(last, next);
	}
	else last = a;
    }
    return ans;
}


/* The change to lists based on dotted pairs has meant that it was
   necessary to separate the internal code for "c" and "unlist".
   Although the functions are quite similar, they operate on very
   different data structures.
*/

/* The major difference between the two functions is that the value of
   the "recursive" argument is FALSE by default for "c" and TRUE for
   "unlist".  In addition, "c" takes ... while "unlist" takes a single
   argument.
*/

/* This is a primitive SPECIALSXP */
HIDDEN SEXP do_c(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* Attempt method dispatch. */
    ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::EVALUATED);
    // if (DispatchAnyOrEval(call, op, "c", args, env, &ans, 1, 1))
	// //      ^^^ "Any" => all args are Rf_eval()ed and checked => correct multi-arg dispatch
	// return(ans);
    auto dispatched = Rf_Dispatch(SEXP_downcast<const Expression*>(call),
                                  SEXP_downcast<const BuiltInFunction*>(op),
                                  arglist,
                                  SEXP_downcast<Environment*>(env));
    if (dispatched.first)
      return dispatched.second;
    return do_c_dflt(SEXP_downcast<Expression*>(call), SEXP_downcast<BuiltInFunction*>(op), std::move(arglist), env);
}

static RObject* do_c_dflt(Expression* call, BuiltInFunction* op, ArgList&& arglist, RObject* env)
{
    RObject* args = const_cast<PairList*>(arglist.list());


    /* Method dispatch has failed; run the default code. */
    /* By default we do not recurse, but this can be over-ridden */
    /* by an optional "recursive" argument. */

    Rboolean
	usenames = TRUE,
	recurse = FALSE;
    /* this was only done for length(args) > 1 prior to 1.5.0,
       _but_ `recursive' might be the only argument */
    PROTECT(args = c_Extract_opt(args, &recurse, &usenames, call));

    /* Determine the type of the returned value. */
    /* The strategy here is appropriate because the */
    /* object being operated on is a pair based list. */

    struct BindData data;
/*    data.deparse_level = 1;  Initialize this early. */
    data.ans_flags  = 0;
    data.ans_length = 0;
    data.ans_nnames = 0;

    SEXP t, ans;
    for (t = args; t != nullptr; t = CDR(t)) {
	if (usenames && !data.ans_nnames) {
	    if (!Rf_isNull(TAG(t))) data.ans_nnames = 1;
	    else data.ans_nnames = HasNames(CAR(t));
	}
	AnswerType(CAR(t), recurse, usenames, &data, call);
    }

    /* If a non-vector argument was encountered (perhaps a list if */
    /* recursive is FALSE) then we must return a list.	Otherwise, */
    /* we use the natural coercion for vector types. */

    SEXPTYPE mode = NILSXP;
    if      (data.ans_flags & 512) mode = EXPRSXP;
    else if (data.ans_flags & 256) mode = VECSXP;
    else if (data.ans_flags & 128) mode = STRSXP;
    else if (data.ans_flags &  64) mode = CPLXSXP;
    else if (data.ans_flags &  32) mode = REALSXP;
    else if (data.ans_flags &  16) mode = INTSXP;
    else if (data.ans_flags &	2) mode = LGLSXP;
    else if (data.ans_flags &	1) mode = RAWSXP;

    /* Allocate the return value and set up to pass through */
    /* the arguments filling in values of the returned object. */

    PROTECT(ans = Rf_allocVector(mode, data.ans_length));
    data.ans_ptr = ans;
    data.ans_length = 0;
    t = args;

    if (mode == VECSXP || mode == EXPRSXP) {
	if (!recurse) {
	    while (args != nullptr) {
		ListAnswer(CAR(args), 0, &data, call);
		args = CDR(args);
	    }
	}
	else ListAnswer(args, recurse, &data, call);
	data.ans_length = Rf_xlength(ans);
    }
    else if (mode == STRSXP)
	StringAnswer(args, &data, call);
    else if (mode == CPLXSXP)
	ComplexAnswer(args, &data, call);
    else if (mode == REALSXP)
	RealAnswer(args, &data, call);
    else if (mode == RAWSXP)
	RawAnswer(args, &data, call);
    else if (mode == LGLSXP)
	LogicalAnswer(args, &data, call);
    else /* integer */
	IntegerAnswer(args, &data, call);
    args = t;

    /* Build and attach the names attribute for the returned object. */

    if (data.ans_nnames && data.ans_length > 0) {
	PROTECT(data.ans_names = Rf_allocVector(STRSXP, data.ans_length));
	data.ans_nnames = 0;
	while (args != nullptr) {
	    struct NameData nameData;
	    nameData.seqno = 0;
	    nameData.count = 0;
	    NewExtractNames(CAR(args), nullptr, TAG(args), recurse, &data, &nameData);
	    args = CDR(args);
	}
	Rf_setAttrib(ans, Symbols::NamesSymbol, data.ans_names);
	UNPROTECT(1);
    }
    UNPROTECT(2);
    R_FreeStringBufferL(&cbuff);
    return ans;
} /* do_c */


HIDDEN SEXP do_unlist(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject*  recursive_, RObject*  use_names_)
{
    SEXP ans, t;
    R_xlen_t i, n = 0;
    struct BindData data;

/*    data.deparse_level = 1; */

    /* By default we recurse, but this can be over-ridden */
    /* by an optional "recursive" argument. */

    SEXP args = PROTECT(x_);
    Rboolean recurse = Rboolean(Rf_asLogical(recursive_));
    Rboolean usenames = Rboolean(Rf_asLogical(use_names_));
    Rboolean lenient = TRUE; // was (implicitly!) FALSE  up to R 3.0.1

    /* Determine the type of the returned value. */
    /* The strategy here is appropriate because the */
    /* object being operated on is a generic vector. */

    data.ans_flags  = 0;
    data.ans_length = 0;
    data.ans_nnames = 0;

    if (Rf_isNewList(args)) {
	n = Rf_xlength(args);
	if (usenames && Rf_getAttrib(args, Symbols::NamesSymbol) != nullptr)
	    data.ans_nnames = 1;
	for (i = 0; i < n; i++) {
	    if (usenames && !data.ans_nnames)
		data.ans_nnames = HasNames(VECTOR_ELT(args, i));
	    AnswerType(VECTOR_ELT(args, i), recurse, usenames, &data, call);
	}
    }
    else if (Rf_isList(args)) {
	for (t = args; t != nullptr; t = CDR(t)) {
	    if (usenames && !data.ans_nnames) {
		if (!Rf_isNull(TAG(t))) data.ans_nnames = 1;
		else data.ans_nnames = HasNames(CAR(t));
	    }
	    AnswerType(CAR(t), recurse, usenames, &data, call);
	}
    }
    else {
	UNPROTECT(1);
	if (lenient || Rf_isVector(args)) return args;
	else Rf_error(_("argument not a list"));
    }

    /* If a non-vector argument was encountered (perhaps a list if */
    /* recursive is FALSE) then we must return a list.  Otherwise, */
    /* we use the natural coercion for vector types. */

    SEXPTYPE mode = NILSXP;
    if      (data.ans_flags & 512) mode = EXPRSXP;
    else if (data.ans_flags & 256) mode = VECSXP;
    else if (data.ans_flags & 128) mode = STRSXP;
    else if (data.ans_flags &  64) mode = CPLXSXP;
    else if (data.ans_flags &  32) mode = REALSXP;
    else if (data.ans_flags &  16) mode = INTSXP;
    else if (data.ans_flags &	2) mode = LGLSXP;
    else if (data.ans_flags &	1) mode = RAWSXP;

    /* Allocate the return value and set up to pass through */
    /* the arguments filling in values of the returned object. */

    PROTECT(ans = Rf_allocVector(mode, data.ans_length));
    data.ans_ptr = ans;
    data.ans_length = 0;
    t = args;

    if (mode == VECSXP || mode == EXPRSXP) {
	if (!recurse) {
	    for (i = 0; i < n; i++)
		ListAnswer(VECTOR_ELT(args, i), 0, &data, call);
	}
	else ListAnswer(args, recurse, &data, call);
	data.ans_length = Rf_xlength(ans);
    }
    else if (mode == STRSXP)
	StringAnswer(args, &data, call);
    else if (mode == CPLXSXP)
	ComplexAnswer(args, &data, call);
    else if (mode == REALSXP)
	RealAnswer(args, &data, call);
    else if (mode == RAWSXP)
	RawAnswer(args, &data, call);
    else if (mode == LGLSXP)
	LogicalAnswer(args, &data, call);
    else /* integer */
	IntegerAnswer(args, &data, call);
    args = t;

    /* Build and attach the names attribute for the returned object. */

    if (data.ans_nnames && data.ans_length > 0) {
	struct NameData nameData;
	PROTECT(data.ans_names = Rf_allocVector(STRSXP, data.ans_length));
	if (!recurse) {
	    if (TYPEOF(args) == VECSXP) {
		SEXP names = Rf_getAttrib(args, Symbols::NamesSymbol);
		data.ans_nnames = 0;
		nameData.seqno = 0;
		nameData.count = 0;
		for (i = 0; i < n; i++) {
		    NewExtractNames(VECTOR_ELT(args, i), nullptr,
				    Rf_ItemName(names, i), recurse, &data, &nameData);
		}
	    }
	    else if (TYPEOF(args) == LISTSXP) {
		data.ans_nnames = 0;
		nameData.seqno = 0;
		nameData.count = 0;
		while (args != nullptr) {
		    NewExtractNames(CAR(args), nullptr,
				    TAG(args), recurse, &data, &nameData);
		    args = CDR(args);
		}
	    }
	}
	else {
	    data.ans_nnames = 0;
	    nameData.seqno = 0;
	    nameData.count = 0;
	    NewExtractNames(args, nullptr, nullptr, recurse, &data, &nameData);
	}
	Rf_setAttrib(ans, Symbols::NamesSymbol, data.ans_names);
	UNPROTECT(1);
    }
    UNPROTECT(2);
    R_FreeStringBufferL(&cbuff);
    return ans;
} /* do_unlist */


/* cbind(deparse.level, ...) and rbind(deparse.level, ...) : */
/* This is a special .Internal */
HIDDEN SEXP do_bind(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP obj, rho;
    FunctionBase* method = nullptr;
    SEXPTYPE mode;
    int deparse_level;
    Rboolean compatible = TRUE, anyS4 = FALSE;
    struct BindData data;
    char buf[512];

    Expression* callx = SEXP_downcast<Expression*>(call);
    Environment* callenv = SEXP_downcast<Environment*>(env);

    /* since R 2.2.0: first argument "deparse.level" */
    deparse_level = Rf_asInteger(Rf_eval(CAR(args), env));
    Rboolean tryS4 = Rboolean(deparse_level >= 0);
    /* NB: negative deparse_level should otherwise be equivalent to deparse_level == 0,
     * --  as cbind(), rbind() below only check for '== 1' and '== 2'
     * {FIXME: methods should do same} */

    /* Lazy evaluation and method dispatch based on argument types are
     * fundamentally incompatible notions.  The results here are
     * ghastly.
     *
     * We build promises to evaluate the arguments and then force the
     * promises so that if we dispatch to a closure below, the closure
     * is still in a position to use "substitute" to get the actual
     * expressions which generated the argument (for naming purposes).
     *
     * The dispatch rule here is as follows:
     *
     * 1) For each argument we get the list of possible class
     *	  memberships from the class attribute.
     *
     * 2) We inspect each class in turn to see if there is an
     *	  applicable method.
     *
     * 3) If we find an applicable method we make sure that it is
     *	  identical to any method determined for prior arguments.
     *	  If it is identical, we proceed, otherwise we immediately
     *	  drop through to the default code.
     */

    ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::RAW);
    arglist.wrapInPromises(SEXP_downcast<Environment*>(env));
    args = nullptr;
    ArgList::const_iterator arg = arglist.getArgs().begin();
    ++arg;  // skip deparse.level
    const ArgList::const_iterator end = arglist.getArgs().end();

    const char *generic = ((PRIMVAL(op) == 1) ? "cbind" : "rbind");
    const char *klass = "";
    for ( ; arg != end; ++arg) {
        PROTECT(obj = Rf_eval(arg->car(), env));
	if (tryS4 && !anyS4 && Rf_isS4(obj)) anyS4 = TRUE;
	if (compatible && Rf_isObject(obj)) {
	    SEXP classlist = PROTECT(R_data_class2(obj));
	    for (int i = 0; i < Rf_length(classlist); i++) {
		const char *s = Rf_translateChar(STRING_ELT(classlist, i));
		if(strlen(generic) + strlen(s) + 2 > 512)
		    Rf_error(_("class name too long in '%s'"), generic);
		sprintf(buf, "%s.%s", generic, s);
		SEXP classmethod = R_LookupMethod(Rf_install(buf), env, env,
						  R_BaseNamespace);
		if (classmethod != R_UnboundValue) {
		    if (klass[0] == '\0') {
			/* There is no previous class */
			/* We use this method. */
			klass = s;
			method = SEXP_downcast<FunctionBase*>(classmethod);
		    }
		    else {
			/* Check compatibility with the */
			/* previous class.  If the two are not */
			/* compatible we drop through to the */
			/* default method. */
			if (strcmp(klass, s)) {
			    method = nullptr;
			    compatible = FALSE;
			}
		    }
		    break; /* go to next parameter */
		}
	    }
	    UNPROTECT(1);
	}
	UNPROTECT(1);
    }

    tryS4 = Rboolean(anyS4 && (!compatible || method == nullptr));
    if (tryS4) {
	// keep 'deparse.level' as first arg and *name* it:
        static Symbol* deparse_level_symbol = Symbol::obtain("deparse.level");
        if (!arglist.getTag(0))
          arglist.setTag(0, deparse_level_symbol);
	// and use methods:::cbind / rbind
	method = findFunction(Symbol::obtain(generic),
                              SEXP_downcast<Environment*>(R_MethodsNamespace));
    } else
      arglist.erase(0); // keeping deparse.level for S4 dispatch
    if (method != nullptr) { // found an S3 or S4 method
	return callx->evaluateFunctionCall(method, callenv, arglist);
    }

    /* Dispatch based on class membership has failed. */
    /* The default code for rbind/cbind.default follows */
    /* First, extract the evaluated arguments. */
    rho = env;
    data.ans_flags = 0;
    data.ans_length = 0;
    data.ans_nnames = 0;
    for (const auto& arg : arglist.getArgs()) {
	AnswerType(PRVALUE(arg.car()), false, false, &data, SEXP_downcast<Expression*>(call));
    }

    /* zero-extent matrices shouldn't give NULL, but cbind(NULL) should: */
    if (!data.ans_flags && !data.ans_length) {
	return nullptr;
    }

    mode = NILSXP;
    if      (data.ans_flags & 512) mode = EXPRSXP;
    else if (data.ans_flags & 256) mode = VECSXP;
    else if (data.ans_flags & 128) mode = STRSXP;
    else if (data.ans_flags &  64) mode = CPLXSXP;
    else if (data.ans_flags &  32) mode = REALSXP;
    else if (data.ans_flags &  16) mode = INTSXP;
    else if (data.ans_flags &	2) mode = LGLSXP;
    else if (data.ans_flags &	1) mode = RAWSXP;

    switch(mode) {
    case NILSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case VECSXP:
    case RAWSXP:
	break;
	/* we don't handle expressions: we could, but coercion of a matrix
	   to an expression is not ideal.
	   FIXME?  had  cbind(y ~ x, 1) work using lists, before */
    default:
	Rf_error(_("cannot create a matrix from type '%s'"),
	      Rf_type2char(mode));
    }

    if (PRIMVAL(op) == 1)
        return cbind(call, arglist, mode, rho, deparse_level);
    else
        return rbind(call, arglist, mode, rho, deparse_level);
}


static void SetRowNames(RObject* dimnames, RObject* x)
{
    if (TYPEOF(dimnames) == VECSXP)
	SET_VECTOR_ELT(dimnames, 0, x);
    else if (TYPEOF(dimnames) == LISTSXP)
	SETCAR(dimnames, x);
}

static void SetColNames(RObject* dimnames, RObject* x)
{
    if (TYPEOF(dimnames) == VECSXP)
	SET_VECTOR_ELT(dimnames, 1, x);
    else if (TYPEOF(dimnames) == LISTSXP)
	SETCADR(dimnames, x);
}

/*
 * Apparently i % 0 could occur here (PR#2541).  But it should not,
 * as zero-length vectors are ignored and
 * zero-length matrices must have zero columns,
 * unless the result has zero rows, hence is of length zero and no
 * copying will be done.
 */
static RObject* cbind(RObject* call, const ArgList& args, SEXPTYPE mode, RObject* rho,
		  int deparse_level)
{
    Rboolean have_rnames = FALSE, have_cnames = FALSE, warned = FALSE;
    int nnames, mnames;
    int rows, cols, mrows, lenmin = 0;
    SEXP dn, u, result, dims;

    nnames = 0;
    mnames = 0;
    rows = 0;
    cols = 0;
    mrows = -1;

    /* check if we are in the zero-row case */

    for (const auto& arg : args.getArgs()) {
        u = PRVALUE(arg.car());
	if((Rf_isMatrix(u) ? Rf_nrows(u) : Rf_length(u)) > 0) {
	    lenmin = 1;
	    break;
	}
    }

    /* check conformability of matrix arguments */

    int na = 0;
    for (const auto& arg : args.getArgs()) {
        u = PRVALUE(arg.car());
	dims = Rf_getAttrib(u, Symbols::DimSymbol);
	if (Rf_length(dims) == 2) {
	    if (mrows == -1)
		mrows = INTEGER(dims)[0];
	    else if (mrows != INTEGER(dims)[0])
		Rf_error(_("number of rows of matrices must match (see arg %d)"),
		      na + 1);
	    cols += INTEGER(dims)[1];
	}
	else if (Rf_length(u) >= lenmin) {
	    rows = std::max(rows, Rf_length(u));
	    cols += 1;
	}
        na++;
    }
    if (mrows != -1) rows = mrows;

    /* Check conformability of vector arguments. -- Look for dimnames. */

    na = 0;
    for (const auto& arg : args.getArgs()) {
        u = PRVALUE(arg.car());
	dims = Rf_getAttrib(u, Symbols::DimSymbol);
	if (Rf_length(dims) == 2) {
	    dn = Rf_getAttrib(u, Symbols::DimNamesSymbol);
	    if (Rf_length(dn) == 2) {
		if (VECTOR_ELT(dn, 1) != nullptr)
		    have_cnames = TRUE;
		if (VECTOR_ELT(dn, 0) != nullptr)
		    mnames = mrows;
	    }
	} else {
	    int k = Rf_length(u);
	    if (!warned && k > 0 && (k > rows || rows % k)) {
		warned = TRUE;
		Rf_warning("number of rows of result is not a multiple of vector length (arg %d)", na + 1);
	    }
	    PROTECT(dn = Rf_getAttrib(u, Symbols::NamesSymbol));
	    if (k >= lenmin && (arg.tag() != nullptr ||
				(deparse_level == 2) ||
				((deparse_level == 1) &&
				 Rf_isSymbol(Rf_substitute(arg.car(), nullptr)))))
		have_cnames = TRUE;
	    nnames = std::max(nnames, Rf_length(dn));
	    UNPROTECT(1); /* dn */
	}
        na++;
    }
    if (mnames || nnames == rows)
	have_rnames = TRUE;

    PROTECT(result = Rf_allocMatrix(mode, rows, cols));
    R_xlen_t n = 0; // index, possibly of long vector

    if (mode == STRSXP) {
      for (const auto& arg : args.getArgs()) {
            u = PRVALUE(arg.car());
	    if (Rf_isMatrix(u) || Rf_length(u) >= lenmin) {
		u = Rf_coerceVector(u, STRSXP);
		R_xlen_t k = XLENGTH(u);
		R_xlen_t idx = (!Rf_isMatrix(u)) ? rows : k;
		xcopyStringWithRecycle(result, u, n, idx, k);
		n += idx;
	    }
	}
    }
    else if (mode == VECSXP) {
        for (const auto& arg : args.getArgs()) {
            u = PRVALUE(arg.car());
	    int umatrix = Rf_isMatrix(u); /* might be lost in coercion to VECSXP */
	    if (umatrix || Rf_length(u) >= lenmin) {
		/* we cannot assume here that coercion will work */
		switch(TYPEOF(u)) {
		case NILSXP:
		case LANGSXP:
		case RAWSXP:
		case LGLSXP:
		case INTSXP:
		case REALSXP:
		case CPLXSXP:
		case STRSXP:
		case VECSXP:
		case LISTSXP:
		{
		    PROTECT(u = Rf_coerceVector(u, mode));
		    R_xlen_t k = XLENGTH(u);
		    if (k > 0) {
			R_xlen_t idx = (!umatrix) ? rows : k;
			R_xlen_t i, i1;
			MOD_ITERATE1(idx, k, i, i1, {
			    SET_VECTOR_ELT(result, n++,
				Rf_lazy_duplicate(VECTOR_ELT(u, i1)));
			});
		    }
		    UNPROTECT(1);
		    break;
		}
		default:
		    for (int i = 0; i < rows; i++)
			SET_VECTOR_ELT(result, n++, Rf_lazy_duplicate(u));
		}
	    }
	}
    }
    else if (mode == CPLXSXP) {
        for (const auto& arg : args.getArgs()) {
            u = PRVALUE(arg.car());
	    if (Rf_isMatrix(u) || Rf_length(u) >= lenmin) {
		u = Rf_coerceVector(u, CPLXSXP);
		R_xlen_t k = XLENGTH(u);
		R_xlen_t idx = (!Rf_isMatrix(u)) ? rows : k;
		xcopyWithRecycle(COMPLEX(result), COMPLEX(u), n, idx, k);
		n += idx;
	    }
	}
    }
    else if (mode == RAWSXP) {
        for (const auto& arg : args.getArgs()) {
            u = PRVALUE(arg.car());
	    if (Rf_isMatrix(u) || Rf_length(u) >= lenmin) {
		u = Rf_coerceVector(u, RAWSXP);
		R_xlen_t k = XLENGTH(u);
		R_xlen_t idx = (!Rf_isMatrix(u)) ? rows : k;
		xcopyWithRecycle(RAW(result), RAW(u), n, idx, k);
		n += idx;
	    }
	}
    }
    else { /* everything else, currently REALSXP, INTSXP, LGLSXP */
        for (const auto& arg : args.getArgs()) {
            u = PRVALUE(arg.car()); /* type of u can be any of: RAW, LGL, INT, REAL, or NULL */
	    if (Rf_isMatrix(u) || Rf_length(u) >= lenmin) {
		R_xlen_t k = Rf_xlength(u); /* use xlength since u can be NULL */
		R_xlen_t idx = (!Rf_isMatrix(u)) ? rows : k;
		if (TYPEOF(u) <= INTSXP) { /* INT or LGL */
		    if (mode <= INTSXP) {
			xcopyWithRecycle(INTEGER(result), INTEGER(u),
						n, idx, k);
			n += idx;
		    }
		    else {
			R_xlen_t i, i1;
			MOD_ITERATE1(idx, k, i, i1, {
			    REAL(result)[n++] =
				(INTEGER(u)[i1]) == NA_INTEGER ? NA_REAL : INTEGER(u)[i1];
			});
		    }
		}
		else if (TYPEOF(u) == REALSXP) {
		    xcopyWithRecycle(REAL(result), REAL(u), n, idx, k);
		    n += idx;
		}
		else { /* RAWSXP */
		    /* FIXME: I'm not sure what the author intended when the sequence was
		       defined as raw < logical -- it is possible to represent logical as
		       raw losslessly but not vice versa. So due to the way this was
		       defined the raw -> logical conversion is bound to be lossy .. */
		    if (mode == LGLSXP) {
			R_xlen_t i, i1;
			MOD_ITERATE1(idx, k, i, i1, {
			    LOGICAL(result)[n++] = RAW(u)[i1] ? TRUE : FALSE;
			});
		    } else {
			R_xlen_t i, i1;
			MOD_ITERATE1(idx, k, i, i1, {
			    INTEGER(result)[n++] = (unsigned char) RAW(u)[i1];
			});
		    }
		}
	    }
	}
    }

    /* Adjustment of dimnames attributes. */
    if (have_cnames || have_rnames) {
	SEXP tnam,v;
        StringVector* nam = nullptr;
	PROTECT(dn = Rf_allocVector(VECSXP, 2));
	if (have_cnames) {
            nam = StringVector::create(cols);
            SET_VECTOR_ELT(dn, 1, nam);
        }
	int j = 0;
        for (const auto& arg : args.getArgs()) {
            u = PRVALUE(arg.car());
	    if (Rf_isMatrix(u)) {
		v = Rf_getAttrib(u, Symbols::DimNamesSymbol);

		if (have_rnames &&
		    Rf_GetRowNames(dn) == nullptr &&
		    Rf_GetRowNames(v) != nullptr)
		    SetRowNames(dn, Rf_lazy_duplicate(Rf_GetRowNames(v)));

		/* rbind() does this only  if(have_?names) .. : */
		/* but if tnam is non-null, have_cnames = TRUE: see above */
		tnam = Rf_GetColNames(v);
		if (tnam != nullptr) {
		    for (int i = 0; i < Rf_length(tnam); i++)
			SET_STRING_ELT(nam, j++, STRING_ELT(tnam, i));
		}
		else if (have_cnames) {
		    for (int i = 0; i < Rf_ncols(u); i++)
			SET_STRING_ELT(nam, j++, R_BlankString);
		}
	    } else if (Rf_length(u) >= lenmin) {
		u = Rf_getAttrib(u, Symbols::NamesSymbol);

		if (have_rnames && Rf_GetRowNames(dn) == nullptr
		    && u != nullptr && Rf_length(u) == rows)
		    SetRowNames(dn, Rf_lazy_duplicate(u));

		if (arg.tag() != nullptr)
                  (*nam)[j++] = const_cast<String*>(
                      SEXP_downcast<const Symbol*>(arg.tag())->name());
		else {
                  GCStackRoot<> expr(Rf_substitute(arg.car(), nullptr));
		    if (deparse_level == 1 && Rf_isSymbol(expr))
			SET_STRING_ELT(nam, j++, PRINTNAME(expr));
		    else if (deparse_level == 2) {
			PROTECT(expr);
			SET_STRING_ELT(nam, j++,
				       STRING_ELT(Rf_deparse1line(expr, TRUE), 0));
			UNPROTECT(1); /* expr */
		    } else if (have_cnames)
			SET_STRING_ELT(nam, j++, R_BlankString);
		}
	    }
	}
	Rf_setAttrib(result, Symbols::DimNamesSymbol, dn);
	UNPROTECT(1);
    }
    UNPROTECT(1);
    return result;
} /* cbind */

static RObject* rbind(RObject* call, const ArgList& args, SEXPTYPE mode, RObject* rho,
		  int deparse_level)
{
    Rboolean have_rnames = FALSE, have_cnames = FALSE, warned = FALSE;
    int nnames, mnames;
    int rows, cols, mcols, lenmin = 0;
    SEXP dn, u, result, dims;

    nnames = 0;
    mnames = 0;
    rows = 0;
    cols = 0;
    mcols = -1;

    /* check if we are in the zero-cols case */

    for (const auto& arg : args.getArgs()) {
        u = PRVALUE(arg.car());
	if((Rf_isMatrix(u) ? Rf_ncols(u) : Rf_length(u)) > 0) {
	    lenmin = 1;
	    break;
	}
    }

    /* check conformability of matrix arguments */

    int na = 0;
    for (const auto& arg : args.getArgs()) {
        u = PRVALUE(arg.car());
	dims = Rf_getAttrib(u, Symbols::DimSymbol);
	if (Rf_length(dims) == 2) {
	    if (mcols == -1)
		mcols = INTEGER(dims)[1];
	    else if (mcols != INTEGER(dims)[1])
		Rf_error(_("number of columns of matrices must match (see arg %d)"),
		      na + 1);
	    rows += INTEGER(dims)[0];
	}
	else if (Rf_length(u) >= lenmin){
	    cols = max(cols, Rf_length(u));
	    rows += 1;
	}
        na++;
    }
    if (mcols != -1) cols = mcols;

    /* Check conformability of vector arguments. -- Look for dimnames. */

    na = 0;
    for (const auto& arg : args.getArgs()) {
        u = PRVALUE(arg.car());
	dims = Rf_getAttrib(u, Symbols::DimSymbol);
	if (Rf_length(dims) == 2) {
	    dn = Rf_getAttrib(u, Symbols::DimNamesSymbol);
	    if (Rf_length(dn) == 2) {
		if (VECTOR_ELT(dn, 0) != nullptr)
		    have_rnames = TRUE;
		if (VECTOR_ELT(dn, 1) != nullptr)
		    mnames = mcols;
	    }
	} else {
	    int k = Rf_length(u);
	    if (!warned && k > 0 && (k > cols || cols % k)) {
		warned = TRUE;
		Rf_warning("number of columns of result is not a multiple of vector length (arg %d)", na + 1);
	    }
	    PROTECT(dn = Rf_getAttrib(u, Symbols::NamesSymbol));
	    if (k >= lenmin && (arg.tag() != nullptr ||
				(deparse_level == 2) ||
				((deparse_level == 1) &&
				 Rf_isSymbol(Rf_substitute(arg.car(), nullptr)))))
		have_rnames = TRUE;
	    nnames = std::max(nnames, Rf_length(dn));
	    UNPROTECT(1); /* dn */
	}
        na++;
    }
    if (mnames || nnames == cols)
	have_cnames = TRUE;

    PROTECT(result = Rf_allocMatrix(mode, rows, cols));
    R_xlen_t n = 0;

    if (mode == STRSXP) {
      for (const auto& arg : args.getArgs()) {
            u = PRVALUE(arg.car());
	    if (Rf_isMatrix(u) || Rf_length(u) >= lenmin) {
		u = Rf_coerceVector(u, STRSXP);
		R_xlen_t k = XLENGTH(u);
		R_xlen_t idx = (Rf_isMatrix(u)) ? Rf_nrows(u) : (k > 0);
		xfillStringMatrixWithRecycle(result, u, n, rows, idx, cols, k);
		n += idx;
	    }
	}
    }
    else if (mode == VECSXP) {
        for (const auto& arg : args.getArgs()) {
            u = PRVALUE(arg.car());
	    int umatrix = Rf_isMatrix(u), urows = umatrix ? Rf_nrows(u) : 1; /* coercing to VECSXP will lose these. PR#15468 */
	    if (umatrix || Rf_length(u) >= lenmin) {
		PROTECT(u = Rf_coerceVector(u, mode));
		R_xlen_t k = XLENGTH(u);
		R_xlen_t idx = umatrix ? urows : (k > 0);
		FILL_MATRIX_ITERATE(n, rows, idx, cols, k)
		    SET_VECTOR_ELT(result, didx,
			Rf_lazy_duplicate(VECTOR_ELT(u, sidx)));
		n += idx;
		UNPROTECT(1);
	    }
	}
    }
    else if (mode == CPLXSXP) {
        for (const auto& arg : args.getArgs()) {
            u = PRVALUE(arg.car());
	    if (Rf_isMatrix(u) || Rf_length(u) >= lenmin) {
		u = Rf_coerceVector(u, CPLXSXP);
		R_xlen_t k = XLENGTH(u);
		R_xlen_t idx = (Rf_isMatrix(u)) ? Rf_nrows(u) : (k > 0);
		xfillMatrixWithRecycle(COMPLEX(result), COMPLEX(u), n, rows, idx, cols, k);
		n += idx;
	    }
	}
    }
    else if (mode == RAWSXP) {
        for (const auto& arg : args.getArgs()) {
            u = PRVALUE(arg.car());
	    if (Rf_isMatrix(u) || Rf_length(u) >= lenmin) {
		u = Rf_coerceVector(u, RAWSXP);
		R_xlen_t k = XLENGTH(u);
		R_xlen_t idx = (Rf_isMatrix(u)) ? Rf_nrows(u) : (k > 0);
		xfillMatrixWithRecycle(RAW(result), RAW(u), n, rows, idx, cols, k);
		n += idx;
	    }
	}
    }
    else { /* everything else, currently REALSXP, INTSXP, LGLSXP */
        for (const auto& arg : args.getArgs()) {
            u = PRVALUE(arg.car()); /* type of u can be any of: RAW, LGL, INT, REAL */
	    if (Rf_isMatrix(u) || Rf_length(u) >= lenmin) {
		R_xlen_t k = XLENGTH(u);
		R_xlen_t idx = (Rf_isMatrix(u)) ? Rf_nrows(u) : (k > 0);
		if (TYPEOF(u) <= INTSXP) {
		    if (mode <= INTSXP) {
			xfillMatrixWithRecycle(INTEGER(result),
						      INTEGER(u), n, rows,
						      idx, cols, k);
			n += idx;
		    }
		    else {
			FILL_MATRIX_ITERATE(n, rows, idx, cols, k)
			    REAL(result)[didx]
				= (INTEGER(u)[sidx]) == NA_INTEGER ? NA_REAL : INTEGER(u)[sidx];
			n += idx;
		    }
		}
		else if (TYPEOF(u) == REALSXP) {
		    xfillMatrixWithRecycle(REAL(result), REAL(u), n, rows, idx, cols, k);
		    n += idx;
		}
		else { /* RAWSXP */
		    if (mode == LGLSXP) {
			FILL_MATRIX_ITERATE(n, rows, idx, cols, k)
			    LOGICAL(result)[didx] = RAW(u)[sidx] ? TRUE : FALSE;
		    } else {
			FILL_MATRIX_ITERATE(n, rows, idx, cols, k)
			    INTEGER(result)[didx] = (unsigned char) RAW(u)[sidx];
		    }
		}
	    }
	}
    }

    /* Adjustment of dimnames attributes. */
    if (have_rnames || have_cnames) {
	SEXP tnam,v;
        StringVector* nam = nullptr;
	PROTECT(dn = Rf_allocVector(VECSXP, 2));
	if (have_rnames) {
            nam = StringVector::create(rows);
            SET_VECTOR_ELT(dn, 0, nam);
        }
	int j = 0;
        for (const auto& arg : args.getArgs()) {
            u = PRVALUE(arg.car());
	    if (Rf_isMatrix(u)) {
		v = Rf_getAttrib(u, Symbols::DimNamesSymbol);

		if (have_cnames &&
		    Rf_GetColNames(dn) == nullptr &&
		    Rf_GetColNames(v) != nullptr)
		    SetColNames(dn, Rf_lazy_duplicate(Rf_GetColNames(v)));

		/* cbind() doesn't test have_?names BEFORE tnam!=Nil..:*/
		/* but if tnam is non-null, have_rnames = TRUE: see above */
		tnam = Rf_GetRowNames(v);
		if (have_rnames) {
		    if (tnam != nullptr) {
			for (int i = 0; i < Rf_length(tnam); i++)
			    SET_STRING_ELT(nam, j++, STRING_ELT(tnam, i));
		    }
		    else {
			for (int i = 0; i < Rf_nrows(u); i++)
				SET_STRING_ELT(nam, j++, R_BlankString);
		    }
		}
	    } else if (Rf_length(u) >= lenmin) {
		u = Rf_getAttrib(u, Symbols::NamesSymbol);

		if (have_cnames && Rf_GetColNames(dn) == nullptr
		    && u != nullptr && Rf_length(u) == cols)
		    SetColNames(dn, Rf_lazy_duplicate(u));

		if (arg.tag() != nullptr)
                  (*nam)[j++] = const_cast<String*>(
                      SEXP_downcast<const Symbol*>(arg.tag())->name());
		else {
                  GCStackRoot<> expr(Rf_substitute(arg.car(), nullptr));
		    if (deparse_level == 1 && Rf_isSymbol(expr))
			SET_STRING_ELT(nam, j++, PRINTNAME(expr));
		    else if (deparse_level == 2) {
			PROTECT(expr);
			SET_STRING_ELT(nam, j++,
				       STRING_ELT(Rf_deparse1line(expr, TRUE), 0));
			UNPROTECT(1); /* expr */
		    } else if (have_rnames)
			SET_STRING_ELT(nam, j++, R_BlankString);
		}
	    }
	}
	Rf_setAttrib(result, Symbols::DimNamesSymbol, dn);
	UNPROTECT(1);
    }
    UNPROTECT(1);
    return result;
} /* rbind */
