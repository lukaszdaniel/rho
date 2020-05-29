/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001--2017 The R Core Team
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Pulic License as published by
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

#include "rho/RAllocStack.hpp"

/* charToRaw works at byte level, ignores encoding */
HIDDEN SEXP do_charToRaw(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_)
{
    SEXP ans, x = x_;
    int nc;

    if (!Rf_isString(x) || LENGTH(x) == 0)
	Rf_error(_("argument must be a character vector of length 1"));
    if (LENGTH(x) > 1)
	Rf_warning(_("argument should be a character vector of length 1\nall but the first element will be ignored"));
    nc = LENGTH(STRING_ELT(x, 0));
    ans = Rf_allocVector(RAWSXP, nc);
    if (nc) memcpy(RAW(ans), R_CHAR(STRING_ELT(x, 0)), nc);
    return ans;
}

/* <UTF8>  rawToChar should work at byte level */
HIDDEN SEXP do_rawToChar(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* multiple_)
{
    SEXP ans, x = x_;

    if (!isRaw(x))
	Rf_error(_("argument 'x' must be a raw vector"));
    int multiple = Rf_asLogical(multiple_);
    if (multiple == NA_LOGICAL)
	Rf_error(_("argument 'multiple' must be TRUE or FALSE"));
    if (multiple) {
	R_xlen_t i, nc = XLENGTH(x);
	char buf[2];
	buf[1] = '\0';
	PROTECT(ans = Rf_allocVector(STRSXP, nc));
	for (i = 0; i < nc; i++) {
	    buf[0] = char(RAW(x)[i]);
	    SET_STRING_ELT(ans, i, Rf_mkChar(buf));
	}
	/* do we want to copy e.g. names here? */
	UNPROTECT(1);
	return ans;
    } else {
	int i, j, nc = LENGTH(x);
	/* String is not necessarily 0-terminated and may contain nuls.
	   Strip trailing nuls */
	for (i = 0, j = -1; i < nc; i++) if(RAW(x)[i]) j = i;
	nc = j + 1;
	return Rf_ScalarString(
	    Rf_mkCharLenCE(reinterpret_cast<const char *>(RAW(x)), j+1, CE_NATIVE));
    }
}


HIDDEN SEXP do_rawShift(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* n_)
{
    SEXP ans, x = x_;
    int shift = Rf_asInteger(n_);


    if (!isRaw(x))
	Rf_error(_("argument 'x' must be a raw vector"));
    if (shift == NA_INTEGER || shift < -8 || shift > 8)
	Rf_error(_("argument 'shift' must be a small integer"));
    PROTECT(ans = Rf_duplicate(x));
    if (shift > 0)
	for (R_xlen_t i = 0; i < XLENGTH(x); i++)
	    RAW(ans)[i] <<= shift;
    else
	for (R_xlen_t i = 0; i < XLENGTH(x); i++)
	    RAW(ans)[i] >>= (-shift);
    UNPROTECT(1);
    return ans;
}

HIDDEN SEXP do_rawToBits(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_)
{
    SEXP ans, x = x_;
    R_xlen_t i, j = 0;
    unsigned int tmp;

    if (!isRaw(x))
	Rf_error(_("argument 'x' must be a raw vector"));
    PROTECT(ans = Rf_allocVector(RAWSXP, 8*XLENGTH(x)));
    for (i = 0; i < XLENGTH(x); i++) {
	tmp = static_cast<unsigned int>(RAW(x)[i]);
	for (int k = 0; k < 8; k++, tmp >>= 1)
	    RAW(ans)[j++] = tmp & 0x1;
    }
    UNPROTECT(1);
    return ans;
}

HIDDEN SEXP do_intToBits(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_)
{
    SEXP ans, x;
    R_xlen_t i, j = 0;
    unsigned int tmp;

    PROTECT(x = Rf_coerceVector(x_, INTSXP));
    if (!Rf_isInteger(x))
	Rf_error(_("argument 'x' must be an integer vector"));
    PROTECT(ans = Rf_allocVector(RAWSXP, 32*XLENGTH(x)));
    for (i = 0; i < XLENGTH(x); i++) {
	tmp = static_cast<unsigned int>(INTEGER(x)[i]);
	for (int k = 0; k < 32; k++, tmp >>= 1)
	    RAW(ans)[j++] = tmp & 0x1;
    }
    UNPROTECT(2);
    return ans;
}

HIDDEN SEXP do_packBits(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* type_)
{
    SEXP ans, x = x_, stype = type_;
    Rboolean useRaw;
    R_xlen_t i, len = XLENGTH(x), slen;
    int fac;

    if (TYPEOF(x) != RAWSXP && TYPEOF(x) != LGLSXP && TYPEOF(x) != INTSXP)
	Rf_error(_("argument 'x' must be raw, integer or logical"));
    if (!Rf_isString(stype)  || LENGTH(stype) != 1)
	Rf_error(_("argument '%s' must be a character string"), "type");
    useRaw = Rboolean(strcmp(R_CHAR(STRING_ELT(stype, 0)), "integer"));
    fac = useRaw ? 8 : 32;
    if (len% fac)
	Rf_error(_("argument 'x' must be a multiple of %d long"), fac);
    slen = len/fac;
    PROTECT(ans = Rf_allocVector(useRaw ? RAWSXP : INTSXP, slen));
    for (i = 0; i < slen; i++)
	if (useRaw) {
	    Rbyte btmp = 0;
	    for (int k = 7; k >= 0; k--) {
		btmp <<= 1;
		if (isRaw(x))
		    btmp |= RAW(x)[8*i + k] & 0x1;
		else if (Rf_isLogical(x) || Rf_isInteger(x)) {
		    int j = INTEGER(x)[8*i+k];
		    if (j == NA_INTEGER)
			Rf_error(_("argument 'x' must not contain NAs"));
		    btmp |= j & 0x1;
		}
	    }
	    RAW(ans)[i] = btmp;
	} else {
	    unsigned int itmp = 0;
	    for (int k = 31; k >= 0; k--) {
		itmp <<= 1;
		if (isRaw(x))
		    itmp |= RAW(x)[32*i + k] & 0x1;
		else if (Rf_isLogical(x) || Rf_isInteger(x)) {
		    int j = INTEGER(x)[32*i+k];
		    if (j == NA_INTEGER)
			Rf_error(_("argument 'x' must not contain NAs"));
		    itmp |= j & 0x1;
		}
	    }
	    INTEGER(ans)[i] = int(itmp);
	}
    UNPROTECT(1);
    return ans;
}

/* Simplified version for RFC3629 definition of UTF-8 */
static int mbrtoint(int *w, const char *s)
{
    unsigned int byte;
    byte = *(reinterpret_cast<unsigned char *>(const_cast<char *>(s)));

    if (byte == 0) {
	*w = 0;
	return 0;
    } else if (byte < 0xC0) {
	*w = int(byte);
	return 1;
    } else if (byte < 0xE0) {
	if (!s[1]) return -2;
	if ((s[1] & 0xC0) == 0x80) {
	    *w = int((((byte & 0x1F) << 6) | (s[1] & 0x3F)));
	    return 2;
	} else return -1;
    } else if (byte < 0xF0) {
	if (!s[1] || !s[2]) return -2;
	if (((s[1] & 0xC0) == 0x80) && ((s[2] & 0xC0) == 0x80)) {
	    *w = int(((byte & 0x0F) << 12)
			| ((s[1] & 0x3F) << 6) | (s[2] & 0x3F));
	    byte = *w;
	    if (byte >= 0xD800 && byte <= 0xDFFF) return -1; /* surrogate */
	    // Following Corrigendum 9, these are valid in UTF-8
//	    if (byte == 0xFFFE || byte == 0xFFFF) return -1;
	    return 3;
	} else return -1;
    } else if (byte <= 0xF4) { // for RFC3629
	if (!s[1] || !s[2] || !s[3]) return -2;
	if (((s[1] & 0xC0) == 0x80)
	    && ((s[2] & 0xC0) == 0x80)
	    && ((s[3] & 0xC0) == 0x80)) {
	    *w = int(((byte & 0x07) << 18)
			| ((s[1] & 0x3F) << 12)
			| ((s[2] & 0x3F) << 6)
			| (s[3] & 0x3F));
	    byte = *w;
	    return (byte <= 0x10FFFF) ? 4 : -1;
	} else return -1;
    } else return -1;
    /* return -2; not reached */
}

HIDDEN SEXP do_utf8ToInt(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_)
{
    SEXP ans, x = x_;
    int tmp, used = 0; /* -Wall */
    R_xlen_t i, j, nc;

    if (!Rf_isString(x) || LENGTH(x) == 0)
	Rf_error(_("argument must be a character vector of length 1"));
    if (LENGTH(x) > 1)
	Rf_warning(_("argument should be a character vector of length 1\nall but the first element will be ignored"));
    if (STRING_ELT(x, 0) == NA_STRING) return Rf_ScalarInteger(NA_INTEGER);
    const char *s = R_CHAR(STRING_ELT(x, 0));
    if (!utf8Valid(s)) return Rf_ScalarInteger(NA_INTEGER);
    nc = XLENGTH(STRING_ELT(x, 0)); /* ints will be shorter */
    int *ians = static_cast<int *>(RHO_alloc(nc, sizeof(int)));
    for (i = 0, j = 0; i < nc; i++) {
	used = mbrtoint(&tmp, s);
	if (used <= 0) break;
	ians[j++] = tmp;
	s += used;
    }
    if (used < 0) Rf_error(_("invalid UTF-8 string"));
    ans = Rf_allocVector(INTSXP, j);
    if (j) memcpy(INTEGER(ans), ians, sizeof(int) * j);
    return ans;
}

/* Based on PCRE, but current Unicode only needs 4 bytes with maximum 0x10ffff */
static const int utf8_table1[] = { 0x7f, 0x7ff, 0xffff, 0x1fffff };
static const int utf8_table2[] = { 0, 0xc0, 0xe0, 0xf0 };

static size_t inttomb(char *s, const int wc)
{
    int i, j;
    unsigned int cvalue = wc;
    char buf[10], *b;

    b = s ? s : buf;
    if (cvalue == 0) {*b = 0; return 0;}
    for (i = 0; i < int(sizeof(utf8_table1)/sizeof(int)); i++)
	if (int(cvalue) <= utf8_table1[i]) break;
    b += i;
    for (j = i; j > 0; j--) {
	*b-- = char((0x80 | (cvalue & 0x3f)));
	cvalue >>= 6;
    }
    *b = char((utf8_table2[i] | cvalue));
    return i + 1;
}

#include <R_ext/RS.h>  /* for Calloc/Free */

HIDDEN SEXP do_intToUtf8(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* multiple_, rho::RObject* allow_surrogate_pairs_)
{
    SEXP ans, x;
    int multiple, s_pair;
    size_t used, len;
    char buf[10], *tmp;

    PROTECT(x = Rf_coerceVector(x_, INTSXP));
    if (!Rf_isInteger(x))
	Rf_error(_("argument 'x' must be an integer vector"));
    multiple = Rf_asLogical(multiple_);
    if (multiple == NA_LOGICAL)
	Rf_error(_("argument 'multiple' must be TRUE or FALSE"));
    s_pair = Rf_asLogical(allow_surrogate_pairs_);
    if (s_pair == NA_LOGICAL)
	Rf_error(_("argument 'allow_surrogate_pairs' must be TRUE or FALSE"));
    if (multiple) {
	if (s_pair)
	    Rf_warning("allow_surrogate_pairs = TRUE is incompatible with multiple = TRUE and will be ignored");
	R_xlen_t i, nc = XLENGTH(x);
	PROTECT(ans = Rf_allocVector(STRSXP, nc));
	for (i = 0; i < nc; i++) {
	    int this_ = INTEGER(x)[i];
	    if (this_ == NA_INTEGER 
		|| (this_ >= 0xD800 && this_ <= 0xDFFF) 
		|| this_ > 0x10FFFF)
		SET_STRING_ELT(ans, i, NA_STRING);
	    else {
		used = inttomb(buf, this_);
		buf[used] = '\0';
		SET_STRING_ELT(ans, i, Rf_mkCharCE(buf, CE_UTF8));
	    }
	}
	/* do we want to copy e.g. names here? */
    } else {
	int i, nc = LENGTH(x);
	Rboolean haveNA = FALSE;
	/* Note that this gives zero length for input '0', so it is omitted */
	for (i = 0, len = 0; i < nc; i++) {
	    int this_ = INTEGER(x)[i];
	    if (this_ == NA_INTEGER 
		|| (this_ >= 0xDC00 && this_ <= 0xDFFF)
		|| this_ > 0x10FFFF) {
		haveNA = TRUE;
		break;
	    }
	    else if (this_ >=  0xD800 && this_ <= 0xDBFF) {
		if(!s_pair || i >= nc-1) {haveNA = TRUE; break;}
		int next = INTEGER(x)[i+1];
		if(next >= 0xDC00 && next <= 0xDFFF) i++;
		else {haveNA = TRUE; break;}
		len += 4; // all points not in the basic plane have length 4
	    } 
	    else
		len += inttomb(NULL, this_);
	}
	if (haveNA) {
	    UNPROTECT(1);
	    return Rf_ScalarString(NA_STRING);
	}
	if (len >= 10000) {
	    tmp = Calloc(len+1, char);
	} else {
	    R_CheckStack2(len+1);
	    tmp = static_cast<char *>(alloca(len+1)); tmp[len] = '\0';
	}
	for (i = 0, len = 0; i < nc; i++) {
	    int this_ = INTEGER(x)[i];
	    if(s_pair && (this_ >=  0xD800 && this_ <= 0xDBFF)) {
		// all the validity checking has already been done.
		int next = INTEGER(x)[++i];
		unsigned int hi = this_ - 0xD800, lo = next - 0xDC00;
		this_ = 0x10000 + (hi << 10) + lo;
	    }
	    used = inttomb(buf, this_);
	    strncpy(tmp + len, buf, used);
	    len += used;
	}
	ans = PROTECT(Rf_ScalarString(Rf_mkCharLenCE(tmp, int(len), CE_UTF8)));
	if(len >= 10000) Free(tmp);
    }
    UNPROTECT(2);
    return ans;
}
